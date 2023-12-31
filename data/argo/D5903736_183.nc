CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:58Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121041158  20190604094027  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�/�&�1   @�0D�s�@4w�O�;d�d߅�Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�\D�)D�W\D�s�D�� D�fD�^�D�}D���D� �D�XRD���D�ʏD�D�C3DچfDౚD��D�%qD�m�D�t{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�\B�\B��)B���B��)B��)B��)B��)B��)B��)B��)B��)B�\B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C(�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D �D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(�D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt�RDy��D�	�D�UD�q�D���D�)D�\{D�z�D�њD���D�VD��RD��RD��D�@�Dڄ)D�]D� RD�#4D�k�D�r>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���Aմ9A�x�A��mA�;dA�1A��;AҶFAҮAқ�A�v�A�VA�M�A�I�A�I�A�I�A�G�A�$�A�VA�A��mA��
A���A�ȴA���AѼjAћ�A�z�A�I�AЁA�?}AΣ�A�t�A���Ạ�A�dZA�oAʬA�%AɍPA��A�JAǲ-A�^5A�G�A�$�A�M�AľwA�bNA���A\A�ȴA��hA��yA���A�E�A��jA��PA�
=A�C�A��A�I�A�1'A�ĜA�hsA�ƨA�JA���A�&�A���A��A��\A��hA��7A�r�A� �A� �A���A��
A��A���A���A��A���A� �A��`A�33A�oA���A�5?A�x�A��A���A��RA�=qA��9A��mA���A�ĜA��A��A�E�A���A�?}A��7A�VA��TA�hsA�S�A�"�A���A��A��A�33A��uA�^5A��-A�A���A�S�A�~�A� �AK�A~��A~M�A}�A{�-A{&�Az�/Az��Az~�Ay`BAxQ�Aw�hAvr�As�;Aq�;ApI�Al=qAk�Aj^5AiG�AfjAdbAb^5Aa�hA`��A`�!A`��A`bNA`5?A_�;A_+A]�A\-AZVAXI�AW"�AVZAU?}ASoAQ�TAQdZAP�HAO\)AL�`AKhsAI/AG��AF5?AE�^AEl�AEVADE�AA�
A?t�A=��A=�A<��A<M�A;p�A8�A6r�A5��A5��A533A4��A4��A4v�A3��A2�RA2n�A2$�A0�A/��A.��A-7LA+��A*�uA)��A'��A&��A%��A$I�A#hsA"��A!�PA �A+A��A��A��AQ�A�A�9A9XA�Az�A��A��A��A��AjA�A�A�A5?A�^A5?AA
�A
�!A
��A
�\A
=qA
$�A	��A	�A	��A	�A	�-A	XA��A�AƨA&�A"�A��@��^@�{@�
=@�X@��@�;d@��@��T@�w@�@@�
=@�5?@�/@���@�dZ@�/@��T@���@�-@ܴ9@��
@�ȴ@ج@�K�@և+@�X@�9X@�o@�=q@�&�@�I�@Ϯ@���@��T@̓u@��@���@�O�@�b@ǅ@�K�@��y@�ff@�`B@ě�@�(�@�|�@���@�=q@���@�1'@��H@�n�@���@�%@�bN@�@�v�@���@�bN@� �@��9@�j@���@��@���@�$�@��@�r�@��F@�+@�V@�$�@�5?@�$�@�x�@��@��/@�Z@��@�|�@�l�@�|�@�|�@�@��R@�M�@�E�@��@���@��7@�&�@���@��/@��`@��@���@��@��@�A�@���@��F@�K�@��\@��^@��@���@��@�I�@���@���@��@��;@��;@��w@���@�33@�@��h@��7@�p�@�G�@���@�o@�^5@���@��@���@��@���@�O�@���@��j@���@��D@�r�@�r�@�r�@��@��@��@�r�@�Z@� �@��@��m@��m@�dZ@��\@�{@��@��h@�/@��@���@���@�bN@�9X@��@�1@�  @��@���@���@�t�@�K�@�33@�o@��@���@��!@���@�v�@�$�@��@���@���@�X@�/@��9@�Q�@�(�@�b@�ƨ@��@�S�@�C�@�"�@��H@���@�M�@�{@��@���@��@�/@���@�A�@��@��w@��P@�dZ@�C�@�;d@�@�^5@�ff@�V@��@�{@��@���@���@�x�@�G�@��`@�9X@��w@�|�@�l�@�;d@�
=@��@��R@���@��\@�v�@�E�@�@��`@��D@�Z@��@��;@��@�S�@�S@��@{�F@m�9@f~�@^�}@Up�@M�-@H�Y@=��@9��@2��@*��@&3�@ Xy@�@�3@e�@�j@��@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A���A���A���Aմ9A�x�A��mA�;dA�1A��;AҶFAҮAқ�A�v�A�VA�M�A�I�A�I�A�I�A�G�A�$�A�VA�A��mA��
A���A�ȴA���AѼjAћ�A�z�A�I�AЁA�?}AΣ�A�t�A���Ạ�A�dZA�oAʬA�%AɍPA��A�JAǲ-A�^5A�G�A�$�A�M�AľwA�bNA���A\A�ȴA��hA��yA���A�E�A��jA��PA�
=A�C�A��A�I�A�1'A�ĜA�hsA�ƨA�JA���A�&�A���A��A��\A��hA��7A�r�A� �A� �A���A��
A��A���A���A��A���A� �A��`A�33A�oA���A�5?A�x�A��A���A��RA�=qA��9A��mA���A�ĜA��A��A�E�A���A�?}A��7A�VA��TA�hsA�S�A�"�A���A��A��A�33A��uA�^5A��-A�A���A�S�A�~�A� �AK�A~��A~M�A}�A{�-A{&�Az�/Az��Az~�Ay`BAxQ�Aw�hAvr�As�;Aq�;ApI�Al=qAk�Aj^5AiG�AfjAdbAb^5Aa�hA`��A`�!A`��A`bNA`5?A_�;A_+A]�A\-AZVAXI�AW"�AVZAU?}ASoAQ�TAQdZAP�HAO\)AL�`AKhsAI/AG��AF5?AE�^AEl�AEVADE�AA�
A?t�A=��A=�A<��A<M�A;p�A8�A6r�A5��A5��A533A4��A4��A4v�A3��A2�RA2n�A2$�A0�A/��A.��A-7LA+��A*�uA)��A'��A&��A%��A$I�A#hsA"��A!�PA �A+A��A��A��AQ�A�A�9A9XA�Az�A��A��A��A��AjA�A�A�A5?A�^A5?AA
�A
�!A
��A
�\A
=qA
$�A	��A	�A	��A	�A	�-A	XA��A�AƨA&�A"�A��@��^@�{@�
=@�X@��@�;d@��@��T@�w@�@@�
=@�5?@�/@���@�dZ@�/@��T@���@�-@ܴ9@��
@�ȴ@ج@�K�@և+@�X@�9X@�o@�=q@�&�@�I�@Ϯ@���@��T@̓u@��@���@�O�@�b@ǅ@�K�@��y@�ff@�`B@ě�@�(�@�|�@���@�=q@���@�1'@��H@�n�@���@�%@�bN@�@�v�@���@�bN@� �@��9@�j@���@��@���@�$�@��@�r�@��F@�+@�V@�$�@�5?@�$�@�x�@��@��/@�Z@��@�|�@�l�@�|�@�|�@�@��R@�M�@�E�@��@���@��7@�&�@���@��/@��`@��@���@��@��@�A�@���@��F@�K�@��\@��^@��@���@��@�I�@���@���@��@��;@��;@��w@���@�33@�@��h@��7@�p�@�G�@���@�o@�^5@���@��@���@��@���@�O�@���@��j@���@��D@�r�@�r�@�r�@��@��@��@�r�@�Z@� �@��@��m@��m@�dZ@��\@�{@��@��h@�/@��@���@���@�bN@�9X@��@�1@�  @��@���@���@�t�@�K�@�33@�o@��@���@��!@���@�v�@�$�@��@���@���@�X@�/@��9@�Q�@�(�@�b@�ƨ@��@�S�@�C�@�"�@��H@���@�M�@�{@��@���@��@�/@���@�A�@��@��w@��P@�dZ@�C�@�;d@�@�^5@�ff@�V@��@�{@��@���@���@�x�@�G�@��`@�9X@��w@�|�@�l�@�;d@�
=@��@��R@���@��\@�v�@�E�@�@��`@��D@�Z@��@��;@��G�O�@�S@��@{�F@m�9@f~�@^�}@Up�@M�-@H�Y@=��@9��@2��@*��@&3�@ Xy@�@�3@e�@�j@��@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�uB
�uB
�uB
�uB
�oB
�bB
�JB
�B
�B
� B
~�B
~�B
~�B
~�B
}�B
}�B
|�B
|�B
|�B
|�B
|�B
{�B
{�B
z�B
z�B
{�B
}�B
}�B
~�B
~�B
�=B
��B
�B
�mB:^B]/BhsB�B�B��B�
B�;B�B�B�B  BBDBPBVB�B(�B9XB>wBA�BL�BN�BXB_;B]/BYBZB_;BgmBdZBdZBhsBk�Bl�BiyBffBdZBbNBhsBu�B�B�PB��B�?B�LB�LB�B��B��B��B��B�uB~�By�Bv�Bq�Bp�BiyBXB9XB%�B�B	7B��B�mB�5B�B��B�?B�B��B��B�BgmB^5B[#BR�BD�B6FB.B$�B�B	7B
��B
�yB
�#B
��B
�qB
�^B
�B
�uB
�=B
�B
�B
y�B
q�B
n�B
p�B
t�B
r�B
ffB
ZB
VB
T�B
C�B
33B
&�B
VB
%B
  B	��B	�ZB	�B	��B	��B	ȴB	ǮB	ǮB	ŢB	ĜB	��B	�jB	�3B	��B	��B	�uB	�VB	�=B	�B	y�B	r�B	o�B	k�B	cTB	W
B	N�B	E�B	?}B	7LB	49B	33B	0!B	)�B	�B	\B		7B	%B	%B	B��B�B�B�B�yB�sB�mB�fB�`B�TB�;B�5B�)B�
B��B��BǮBB�wB�^B�?B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B�B�!B�-B�-B�'B�-B�3B�-B�B�B�3B�dBÖBȴBȴBɺB��B��B��B��B��B��B��B�LB�LB�LB��B�DB�oB��B�3B�LB�LB�9B��B��B��B��B��B��B��B��B��B��B�{B�\B�PB�VB�\B�bB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�FB�LB�dB�qB�wB��BBŢBȴB��B��B��B��B��B�
B�B�B�B�)B�5B�NB�sB�B�B�B��B��B��B��B��B��B��B	B	%B	%B	
=B	DB	PB	\B	hB	�B	�B	�B	�B	�B	 �B	 �B	!�B	#�B	%�B	(�B	(�B	+B	-B	/B	1'B	5?B	6FB	6FB	5?B	5?B	5?B	9XB	;dB	?}B	C�B	G�B	K�B	P�B	VB	ZB	^5B	bNB	e`B	hsB	l�B	p�B	r�B	s�B	t�B	v�B	~�B	�B	� B	� B	� B	}�B	z�B	|�B	� B	�B	�B	�B	�+B	�=B	�JB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�9B	�9B	�?B	�LB	�LB	�RB	�XB	�^B	�^B	�dB	�dB	�jB	�jB	�qB	�qB	�}B	��B	��B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�5B	�;B	�BB	�HB	�NB	�TB	�ZB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
%B
�B
�B
%�B
0�B
6�B
<PB
C�B
JrB
N�B
TB
W�B
]�B
b�B
fLB
h
B
j�B
qAB
utB
u�B
yrB
z^111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�@B
�>B
�@B
�@B
�<B
�,B
�B
{�B
x�B
w�B
v�B
v�B
v�B
v�B
u�B
u�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
r�B
r�B
s�B
u�B
u�B
v�B
v�B
�B
�^B
��B
�3B2BT�B`4B{�B��BȜBοB��B�DB�;B�eB��B��B�BBB=B �B1B6-B98BD~BF�BO�BV�BT�BP�BQ�BV�B_B\B\B`%Bc9Bd:Ba+B^B\BZB`%BmwB|�B�B�=B��B��B��B��B��B��B�cB�TB�'Bv�Bq�BnzBi^BhRBa-BO�B1B�B;B �B�B�,B��B��BÈB�B��B��B�OB{�B_5BU�BR�BJ�B<eB.B%�B�BpBB
�B
�KB
��B
ÝB
�BB
�6B
��B
�MB
�B
|�B
y�B
q�B
i�B
fqB
h�B
l�B
j�B
^AB
Q�B
M�B
L�B
;wB
+B
�B
8B	�B	��B	�B	�>B	��B	��B	îB	��B	��B	��B	��B	��B	�tB	�RB	�B	��B	��B	�^B	�CB	�+B	{B	q�B	j�B	g�B	cvB	[BB	N�B	F�B	=�B	7pB	/@B	,)B	+'B	(B	!�B	�B	UB	/B�B�B�B��B�B�B�yB�sB�lB�gB�_B�YB�NB�5B�2B�$B�B��B��B��B��B�uB�\B�@B�,B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�	B�$B�-B�.B�&B�0B�3B�1B�B�B�7B�gB��B��B��B��B��B��B��B��B��B��B��B�NB�NB�PB��B�JB�sB��B�6B�OB�QB�<B��B��B��B��B��B��B��B��B��B��B��B�eB�UB�^B�`B�jB�|B��B��B��B��B��B��B��B��B��B��B��B�B�#B�LB�SB�oB�uB�{B��B��B��B��B��B��B��B�B��B�B�B�B�B�.B�8B�RB�yB�B�B�B��B��B��B��B��B��B��B�B�'B�'B	AB	HB	RB	]B		mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	#B	%B	'B	)*B	-CB	.EB	.HB	-BB	-CB	-DB	1VB	3fB	7~B	;�B	?�B	C�B	H�B	NB	RB	V4B	ZMB	]dB	`qB	d�B	h�B	j�B	k�B	l�B	n�B	v�B	yB	w�B	w�B	w�B	u�B	r�B	t�B	w�B	yB	zB	}B	)B	�;B	�EB	�SB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�*B	�5B	�1B	�;B	�EB	�GB	�LB	�QB	�XB	�VB	�]B	�]B	�gB	�bB	�lB	�kB	�xB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�(B	�-B	�4B	�8B	�@B	�FB	�GB	�SB	�jB	�wB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B
�B
�B
�B
(�B
.�B
4CB
;�B
BeB
F�B
L
B
O�B
U�B
Z�B
^@B
_�B
b�B
i2B
mgB
m�B
qgB
rV111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.008(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940272019060409402720190604094027  AO  ARCAADJP                                                                    20181121041158    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041158  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041158  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094027  IP                  G�O�G�O�G�O�                