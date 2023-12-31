CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-10-11T07:01:31Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20171011070131  20190604094030  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�,�ԓ(�1   @�,�lwژ@5�I�^5�d�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�33B���B���B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D�
�D�>D�o\D��D���D�%D��qD���D��D�A�D���D���D�3D�:�DچD���D�{D�B�D�j�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB�\B�\B���B���B��)B��)B��)B��)B�\B�\B��)B���B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�DtۅDy�RD��D�;�D�mD���D��{D�"�D��4D�ҐD�gD�?]D�]D�ȤD� �D�8RDڃ�D�ȤD�>D�@RD�hRD��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A܋DA܉7A�|�A�|�A�~�A�|�A�~�A�z�A܅A܁A�ffA�O�A�33A�-A�&�A�$�A�$�A�$�A�"�A��A�VA���A��yA��A���A۾wAۼjA۶FA۬Aۛ�AۓuAۏ\AۅA�7LA��AجA�t�A�ĜAω7A��A���A͸RA��mA�5?A�r�A�A�bAƓuA��#A���A²-A�|�A���A��FA���A�;dA�7LA���A�O�A���A�\)A��A�bA��A�Q�A��A�+A���A���A�ȴA���A���A���A���A��A���A��9A�=qA��A�hsA�A�A��^A��A�t�A�S�A�I�A�%A��A�(�A�~�A�Q�A��A�5?A�JA���A�bNA�
=A��hA�`BA��\A�bNA�1'A��!A���A�33A��!A���A��7A���A�33A�\)A��A��HA� �A���A�`BA�M�A�-A�E�A�A�A��DA��A��DA�K�A���A�JA� �A�ĜA~�HAz��Av=qAs%Aq%ApbAo�Am�wAiK�Ad �A_�A]AZ�\AX�RAV��AT��AP�`AN�AMK�AL(�AKt�AJ��AJ-AG��AFjAE��AE`BADQ�AA��AA�A@ZA?33A>JA<�A<I�A;��A:1'A8�uA7t�A6�A4�A2�yA1�;A1�^A1K�A0z�A/VA-;dA+�A*�uA(��A'��A&bA$��A#C�A"n�A!�#A!�A!�A�-AhsA�A�A�TA�Av�A�A�7AZAx�A�uA1A�Av�AXAȴA{At�A�A��A��A �Ax�A=qAl�A5?A
�9A
=qA��A��A`BA"�A�!A$�A�FAO�A�+A��A��A��@��@��j@�C�@�n�@�Ĝ@�K�@�~�@�5?@��-@�@��@��;@�V@��@�P@�\)@�^5@�K�@���@���@���@�9X@߾w@�bN@�1'@�|�@�`B@���@�(�@��H@٩�@ׅ@�M�@թ�@�hs@��@�S�@�J@��`@�t�@��y@Ο�@���@�7L@�A�@�t�@�ȴ@��@Ɂ@�Ĝ@�l�@��@ģ�@���@�"�@��y@��H@\@���@��/@��@���@�K�@��y@�5?@���@�&�@�1@��@�;d@��@��T@��h@�&�@�V@��`@���@��j@��u@�Q�@�C�@�-@�@��h@�O�@��@���@��@�r�@�9X@�1@��
@�|�@�+@���@��@�`B@��`@�z�@���@�~�@��@�@��#@��7@�&�@�bN@�ƨ@�\)@�+@�o@��@�X@���@��@�1'@�  @���@���@�|�@��@�^5@�5?@��^@�x�@�G�@��@��/@�Ĝ@��j@���@�r�@��m@��w@���@���@��@�33@��H@��H@��H@��@��@��@��!@�M�@��@�@��-@��h@�x�@�O�@���@�1'@�ƨ@��F@���@�C�@�"�@�
=@��@��R@��\@���@���@�M�@�@�hs@��/@��j@��/@���@��@��D@� �@���@�S�@��@��!@�~�@�5?@���@���@��@�O�@��@���@��9@��@�(�@�  @��w@�l�@��y@���@�~�@�V@�$�@��T@��^@�`B@��@���@��9@���@�r�@�1'@�ƨ@�l�@��@���@���@�V@�-@�{@���@�X@�&�@���@���@��9@���@�r�@�j@�A�@� �@�  @��w@��F@��F@��P@�;d@�o@��y@��R@���@�v�@�-@�@���@���@��@�O�@��/@�z�@�Q�@�I�@�A�@�9X@��@�  @���@�1@�  @��@�ƨ@��w@��w@��P@�\)@�K�@��"@�ߤ@�	l@{�@p�O@j��@e?}@]�#@S�+@M�@H�	@E�3@>e@6R�@/P�@(�@#�*@�7@o @~(@`B111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A܋DA܉7A�|�A�|�A�~�A�|�A�~�A�z�A܅A܁A�ffA�O�A�33A�-A�&�A�$�A�$�A�$�A�"�A��A�VA���A��yA��A���A۾wAۼjA۶FA۬Aۛ�AۓuAۏ\AۅA�7LA��AجA�t�A�ĜAω7A��A���A͸RA��mA�5?A�r�A�A�bAƓuA��#A���A²-A�|�A���A��FA���A�;dA�7LA���A�O�A���A�\)A��A�bA��A�Q�A��A�+A���A���A�ȴA���A���A���A���A��A���A��9A�=qA��A�hsA�A�A��^A��A�t�A�S�A�I�A�%A��A�(�A�~�A�Q�A��A�5?A�JA���A�bNA�
=A��hA�`BA��\A�bNA�1'A��!A���A�33A��!A���A��7A���A�33A�\)A��A��HA� �A���A�`BA�M�A�-A�E�A�A�A��DA��A��DA�K�A���A�JA� �A�ĜA~�HAz��Av=qAs%Aq%ApbAo�Am�wAiK�Ad �A_�A]AZ�\AX�RAV��AT��AP�`AN�AMK�AL(�AKt�AJ��AJ-AG��AFjAE��AE`BADQ�AA��AA�A@ZA?33A>JA<�A<I�A;��A:1'A8�uA7t�A6�A4�A2�yA1�;A1�^A1K�A0z�A/VA-;dA+�A*�uA(��A'��A&bA$��A#C�A"n�A!�#A!�A!�A�-AhsA�A�A�TA�Av�A�A�7AZAx�A�uA1A�Av�AXAȴA{At�A�A��A��A �Ax�A=qAl�A5?A
�9A
=qA��A��A`BA"�A�!A$�A�FAO�A�+A��A��A��@��@��j@�C�@�n�@�Ĝ@�K�@�~�@�5?@��-@�@��@��;@�V@��@�P@�\)@�^5@�K�@���@���@���@�9X@߾w@�bN@�1'@�|�@�`B@���@�(�@��H@٩�@ׅ@�M�@թ�@�hs@��@�S�@�J@��`@�t�@��y@Ο�@���@�7L@�A�@�t�@�ȴ@��@Ɂ@�Ĝ@�l�@��@ģ�@���@�"�@��y@��H@\@���@��/@��@���@�K�@��y@�5?@���@�&�@�1@��@�;d@��@��T@��h@�&�@�V@��`@���@��j@��u@�Q�@�C�@�-@�@��h@�O�@��@���@��@�r�@�9X@�1@��
@�|�@�+@���@��@�`B@��`@�z�@���@�~�@��@�@��#@��7@�&�@�bN@�ƨ@�\)@�+@�o@��@�X@���@��@�1'@�  @���@���@�|�@��@�^5@�5?@��^@�x�@�G�@��@��/@�Ĝ@��j@���@�r�@��m@��w@���@���@��@�33@��H@��H@��H@��@��@��@��!@�M�@��@�@��-@��h@�x�@�O�@���@�1'@�ƨ@��F@���@�C�@�"�@�
=@��@��R@��\@���@���@�M�@�@�hs@��/@��j@��/@���@��@��D@� �@���@�S�@��@��!@�~�@�5?@���@���@��@�O�@��@���@��9@��@�(�@�  @��w@�l�@��y@���@�~�@�V@�$�@��T@��^@�`B@��@���@��9@���@�r�@�1'@�ƨ@�l�@��@���@���@�V@�-@�{@���@�X@�&�@���@���@��9@���@�r�@�j@�A�@� �@�  @��w@��F@��F@��P@�;d@�o@��y@��R@���@�v�@�-@�@���@���@��@�O�@��/@�z�@�Q�@�I�@�A�@�9X@��@�  @���@�1@�  @��@�ƨ@��w@��w@��P@�\)G�O�@��"@�ߤ@�	l@{�@p�O@j��@e?}@]�#@S�+@M�@H�	@E�3@>e@6R�@/P�@(�@#�*@�7@o @~(@`B111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�/B�5B�;B�BB�HB�NB�HB�HB�BB�;B�;B�5B�/B�)B�/B�5B�5B�5B�BB�BB�5B�B�;B�mB�sB�mB�B��BB1BVBhB!�B&�B-B9XB=qBO�BW
B[#Be`Bk�Bl�Bm�Bo�By�B�=B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�DB�%B}�Br�Bk�BcTB_;B[#BS�BH�B:^B5?B33B/B,B%�B�B\BB��B�yB�B��BŢB�qB��B��B�VB�By�Bt�BffB]/BT�BO�BA�B2-B$�B�BJB
��B
�B
�5B
ƨB
�!B
��B
�7B
ZB
:^B
�B
B	��B	�B	�B	�HB	ÖB	��B	�DB	z�B	gmB	^5B	XB	N�B	33B	'�B	 �B	�B	�B	�B	�B	hB	DB	1B	B��B��B��B��B�B�B�fB�TB�;B�B��B��B��BƨBBÖBĜBĜBÖB��B�^B�FB�!B�B��B��B��B��B��B��B�{B�uB�hB�oB�bB�bB�PB�JB�DB�=B�7B�1B�%B�B�B� B}�B|�B|�B|�B|�B|�B{�Bz�Bx�Bv�Bs�Bq�Bo�Bo�Bm�Bn�Bn�Bn�Bn�Bn�Bn�Bm�Bl�Bk�BhsBgmBgmBgmBiyBjBjBjBk�Bk�BjBiyBhsBgmBgmBffBcTBbNBaHBaHB]/BZBXBVBVBXBaHBe`BcTBjBiyBjBjBk�Bo�Bq�Br�Br�Bu�Bu�Bw�Bz�B}�B� B�B�B�B�1B�=B�JB�\B�\B�hB�uB��B��B��B��B��B��B��B��B��B�B�B�'B�3B�LB�dB�qBÖBŢBƨB��B��B��B��B��B��B��B��B��B��B�
B�#B�/B�5B�5B�BB�BB�BB�HB�HB�HB�NB�TB�`B�mB�B�B�B�B��B��B��B��B��B��B	  B	B	1B	
=B	DB	DB	\B	uB	{B	�B	�B	�B	!�B	#�B	#�B	(�B	+B	+B	+B	+B	+B	,B	-B	.B	/B	/B	0!B	2-B	5?B	6FB	6FB	7LB	;dB	>wB	?}B	?}B	@�B	@�B	A�B	B�B	E�B	F�B	G�B	I�B	K�B	L�B	N�B	P�B	W
B	\)B	_;B	dZB	l�B	n�B	p�B	p�B	r�B	w�B	{�B	{�B	}�B	� B	� B	�B	�B	�B	�%B	�+B	�DB	�PB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�?B	�LB	�XB	�^B	�jB	�wB	��B	B	ĜB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�)B	�5B	�;B	�;B	�;B	�HB	�TB	�ZB	�`B	�`B	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
'B
,�B
0�B
5tB
@4B
FYB
JrB
L~B
S[B
ZB
^�B
e�B
i�B
o�B
r-B
u�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�pB�qB�tB�nB�oB�pB�qB�pB�nB�mB�nB�qB�oB�qB�pB�nB�tB�yB�}BԊBՏB֓BՍBՋBԉBӃBӅB�}B�sB�qB�tB�zB�zB�xBԅBԊB�{B�cBӁB۴BܻB۷B��B��B�PB�tB�B�BB0B!RB-�B1�BD!BKLBObBY�B_�B`�Ba�Bc�BnB~~B��B��B��B��B�B�	B�	B�B�B�
B�B�!B�B��B��B��B��B��B�BzfBr8Bf�B_�BW�BS�BOkBH=B<�B.�B)�B'zB#lB VB-B�B�B�gB�B��B�oB�B��B��B�CB��B��By|Bn<BiBZ�BQ�BI^BDDB5�B&�BEB
�B �B
�^B
�B
ңB
�B
��B
�;B
}�B
N�B
.�B
B	��B	�YB	�6B	�B	��B	�!B	�^B	�B	osB	\B	R�B	L�B	CsB	'�B	�B	aB	QB	FB	CB	,B	B��B��B��B�B�zB�mB�ZB�@B�#B�
B��B��B��BɥBŌB�xB�RB�:B�DB�GB�DB�@B�+B�B��B��B��B��B��B�pB�ZB�LB�4B�*B�$B�B�!B�B�B� B��B�B~�B}�B|�Bz�Bx�Bv�Bt�Br�Bq�Bq�Bq�Bq�Bq�Bp�Bo�Bm�Bk�BhkBf^BdUBdSBbHBcPBcTBcRBcRBcPBcOBbKBaFB`>B].B\%B\*B\'B^0B_;B_;B_9B`@B`@B_8B^1B].B\)B\*B[ BXBWBVBVBQ�BN�BL�BJ�BJ�BL�BVBZBXB_?B^5B_?B_9B`DBdWBfgBgmBghBj|Bj�Bl�Bo�Br�Bt�Bu�Bw�By�B|�B~�B�B�B�B�%B�-B�PB�eB�wB��B��B��B��B��B��B��B��B��B��B�B�B�,B�OB�ZB�`B�xB�~BBĖBĕBŝBĖBėBŞBƢB��B��B��B��B��B��B��B��B� B��B��B�B�	B�B�#B�9B�NB�_B�lB�uB�B�B�B�B�B��B��B��B��B��B��B	B	%B		*B	9B	DB	iB	yB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	#�B	$�B	&�B	)�B	*�B	*�B	+�B	0B	3'B	4+B	4+B	52B	52B	68B	7;B	:OB	;UB	<\B	>iB	@vB	A{B	C�B	E�B	K�B	P�B	S�B	YB	a6B	cIB	eQB	eQB	g]B	lxB	p�B	p�B	r�B	t�B	t�B	u�B	w�B	y�B	z�B	{�B	�B	��B	�B	�B	�)B	�1B	�5B	�LB	�ZB	�`B	�dB	�uB	�wB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�.B	�3B	�@B	�HB	�QB	�`B	�mB	�|B	ƎB	ǕB	ȜB	ʦB	ˮB	̲B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�"B	�'B	�%B	�(B	�)B	�'B	�(B	�,B	�5B	�:B	�9B	�HB	�RB	�TB	�SB	�ZB	�VB	�VB	�^B	�cB	�pB	�rB	�lB	�|B	�vB	�tB	�xB	�tG�O�B	�rB	��B
aB
vB
�B
!^B
%�B
*B
4�B
:�B
?B
AB
G�B
N�B
S�B
Z�B
^�B
dlB
f�B
j�B
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0), vertically averaged dS =-0.011(+/-0.002) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940302019060409403020190604094030  AO  ARCAADJP                                                                    20171011070131    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171011070131  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171011070131  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094030  IP                  G�O�G�O�G�O�                