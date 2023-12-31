CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T04:11:57Z creation      
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121041157  20190604094027  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�c�ˊ1   @��\�u@4�G�z��d�+I�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` DyY�D��D�/
D�aHD��{D�=D�A�D�y�D���D�)D�P�D�vfD�g\D��D�=qD�q�D��\D��D�F�D�p D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB`�Bh�Bo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt[�DyUD��D�,�D�_D��>D�  D�?�D�w]D��HD�	�D�N�D�t)D�eD��D�;4D�o�D��D�gD�D{D�m�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��/AӴ9Aә�A�|�A�;dAҲ-Aҩ�Aҝ�Aҝ�Aҕ�AҍPAҋDA҃AҁA�~�A�x�A�r�A�n�A�l�A�hsA�jA�hsA�hsA�dZA�^5A�33A��AѲ-A�t�A�~�AΝ�A�XA�A�A��A��`A�bNA��Aˉ7A��TAʋDA�ffA�Q�A�+A�A�z�A���AȓuA�&�A�VA�VAũ�A�S�A���A�%A�AuA��A��^A�r�A�bA��TA��+A��9A��HA�A��A�\)A��`A�n�A�E�A�K�A��A�A�"�A�t�A�ĜA��A�t�A�E�A�bA�=qA��yA��yA��DA���A���A�;dA���A�I�A���A��yA�oA���A��TA���A�ffA�C�A��jA���A�7LA�VA�ZA��TA�ĜA���A��A�jA���A�ĜA���A�VA��PA���A�t�A��jA��+A��A���A�+A��jA��9A�`BA�(�A��wA�ffA~��A{ƨAy��Aw�Au�AshsAr�9Aq��Aqx�Ao��AnZAlffAj1Agt�AfAd�DAb�yA_�TA]ƨA\��A[��A[AZZAY��AX9XAWVAU�AU"�AT�`ATA�AR�RAPr�AN�/AM��AKƨAHVAGG�AF�AFv�AE
=AA`BAA&�A@-A=�A;�#A:�+A7�A4�yA3"�A2I�A1�;A0�A/ƨA.z�A.$�A-��A-S�A-VA,��A+��A*A((�A&�A&A#��A"�A!��A 1'A�yA��AoA��A�A=qA�A�;A?}AVA��AQ�A��A\)A"�A��A�A��A=qA�
AO�A�AO�A
��A	��A$�A33AĜA�FA��AE�A`BA��A n�@��@�-@���@�|�@��@�j@��@�ff@�33@�dZ@�@�  @��#@�@�j@�9X@@�o@�$�@��T@�@��-@���@�7@��@�  @ꗍ@��/@畁@��@�7@��@�ȴ@��@��@�E�@�1@�=q@���@���@ج@�z�@�9X@ם�@�x�@�ƨ@ӝ�@ӕ�@�"�@Ұ!@�V@�j@�dZ@�@ͺ^@�`B@�/@��/@̴9@�z�@��@��@ə�@�r�@�K�@��@ƸR@�E�@���@ř�@�O�@�&�@��/@��
@�@°!@��@�`B@�Z@�9X@��@���@�"�@���@�ff@�M�@���@�G�@��/@�b@���@��@�Z@��P@��R@�J@�x�@�bN@�ƨ@�C�@��R@��R@�ff@�&�@��@��m@��
@�ƨ@���@���@�^5@���@���@��h@��@�hs@�X@�G�@�&�@��`@��@� �@��;@��@�K�@�33@��@��@��H@���@��!@�E�@�/@���@��u@�b@��w@�l�@�
=@�ȴ@���@��H@��\@�V@�5?@���@�p�@�7L@�Ĝ@��@�J@���@���@�`B@�V@��@��@���@��w@��@�o@�E�@��@�%@�Ĝ@�j@�Z@�Z@�(�@��@��;@��
@�ƨ@�\)@�
=@��H@��!@���@�v�@�ff@�v�@�v�@�v�@�n�@�$�@�@���@��7@�p�@�&�@��@���@��`@�Ĝ@�9X@���@�"�@�@���@�ff@�^5@�M�@�E�@�{@��@���@��@���@���@�bN@�(�@�b@�1@�b@�b@���@���@��P@��@�|�@�dZ@�C�@�+@��@��R@�V@�-@��h@�O�@��@��@��u@��u@��D@��D@��D@��D@�z�@�bN@�bN@�I�@��@��@��y@��H@��R@�n�@�=q@�@���@���@�p�@�&�@�r�@�9X@� �@��m@��
@��P@�dZ@�h�@���@�4@x�K@qc�@h[�@a(�@Xی@PC-@HI�@@ѷ@97L@3�@+\)@&@"�,@�.@�_@�*@F@V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A���A��/AӴ9Aә�A�|�A�;dAҲ-Aҩ�Aҝ�Aҝ�Aҕ�AҍPAҋDA҃AҁA�~�A�x�A�r�A�n�A�l�A�hsA�jA�hsA�hsA�dZA�^5A�33A��AѲ-A�t�A�~�AΝ�A�XA�A�A��A��`A�bNA��Aˉ7A��TAʋDA�ffA�Q�A�+A�A�z�A���AȓuA�&�A�VA�VAũ�A�S�A���A�%A�AuA��A��^A�r�A�bA��TA��+A��9A��HA�A��A�\)A��`A�n�A�E�A�K�A��A�A�"�A�t�A�ĜA��A�t�A�E�A�bA�=qA��yA��yA��DA���A���A�;dA���A�I�A���A��yA�oA���A��TA���A�ffA�C�A��jA���A�7LA�VA�ZA��TA�ĜA���A��A�jA���A�ĜA���A�VA��PA���A�t�A��jA��+A��A���A�+A��jA��9A�`BA�(�A��wA�ffA~��A{ƨAy��Aw�Au�AshsAr�9Aq��Aqx�Ao��AnZAlffAj1Agt�AfAd�DAb�yA_�TA]ƨA\��A[��A[AZZAY��AX9XAWVAU�AU"�AT�`ATA�AR�RAPr�AN�/AM��AKƨAHVAGG�AF�AFv�AE
=AA`BAA&�A@-A=�A;�#A:�+A7�A4�yA3"�A2I�A1�;A0�A/ƨA.z�A.$�A-��A-S�A-VA,��A+��A*A((�A&�A&A#��A"�A!��A 1'A�yA��AoA��A�A=qA�A�;A?}AVA��AQ�A��A\)A"�A��A�A��A=qA�
AO�A�AO�A
��A	��A$�A33AĜA�FA��AE�A`BA��A n�@��@�-@���@�|�@��@�j@��@�ff@�33@�dZ@�@�  @��#@�@�j@�9X@@�o@�$�@��T@�@��-@���@�7@��@�  @ꗍ@��/@畁@��@�7@��@�ȴ@��@��@�E�@�1@�=q@���@���@ج@�z�@�9X@ם�@�x�@�ƨ@ӝ�@ӕ�@�"�@Ұ!@�V@�j@�dZ@�@ͺ^@�`B@�/@��/@̴9@�z�@��@��@ə�@�r�@�K�@��@ƸR@�E�@���@ř�@�O�@�&�@��/@��
@�@°!@��@�`B@�Z@�9X@��@���@�"�@���@�ff@�M�@���@�G�@��/@�b@���@��@�Z@��P@��R@�J@�x�@�bN@�ƨ@�C�@��R@��R@�ff@�&�@��@��m@��
@�ƨ@���@���@�^5@���@���@��h@��@�hs@�X@�G�@�&�@��`@��@� �@��;@��@�K�@�33@��@��@��H@���@��!@�E�@�/@���@��u@�b@��w@�l�@�
=@�ȴ@���@��H@��\@�V@�5?@���@�p�@�7L@�Ĝ@��@�J@���@���@�`B@�V@��@��@���@��w@��@�o@�E�@��@�%@�Ĝ@�j@�Z@�Z@�(�@��@��;@��
@�ƨ@�\)@�
=@��H@��!@���@�v�@�ff@�v�@�v�@�v�@�n�@�$�@�@���@��7@�p�@�&�@��@���@��`@�Ĝ@�9X@���@�"�@�@���@�ff@�^5@�M�@�E�@�{@��@���@��@���@���@�bN@�(�@�b@�1@�b@�b@���@���@��P@��@�|�@�dZ@�C�@�+@��@��R@�V@�-@��h@�O�@��@��@��u@��u@��D@��D@��D@��D@�z�@�bN@�bN@�I�@��@��@��y@��H@��R@�n�@�=q@�@���@���@�p�@�&�@�r�@�9X@� �@��m@��
@��PG�O�@�h�@���@�4@x�K@qc�@h[�@a(�@Xی@PC-@HI�@@ѷ@97L@3�@+\)@&@"�,@�.@�_@�*@F@V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
q�B
r�B
s�B
t�B
u�B
x�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�1B
�1B
�PB
��B
�B
�jBB|�B�1B�JB�uB��B��B��BĜB�B�B��B+B�B-B6FB6FB33BN�BVBbNBm�Br�B�VB�oB�uB��B��B��B��B��B��B�B�!B�?B�XB�qBƨB��BBŢBƨB��B��B��B��B��BĜB�dB�B��B��B�uB�PB�7B�Bp�BdZBXBL�BH�BB�B<jB33B(�B�B\BB��B�B��B�3B��B�1By�Bl�BcTB\)BO�BD�B9XB-B�BJB
��B
�B
�;B
�
B
��B
��B
�dB
�B
��B
��B
��B
��B
�7B
t�B
ffB
[#B
J�B
=qB
8RB
49B
0!B
(�B
#�B
�B
1B	��B	�B	�;B	��B	ŢB	�XB	�-B	�B	��B	��B	��B	��B	�VB	�+B	�B	� B	y�B	p�B	e`B	]/B	W
B	K�B	>wB	;dB	9XB	6FB	/B	!�B	�B	�B	\B	B��B�B�yB�TB�BB�/B�B��B��B��B��B��B��BȴBĜB�qB�LB�-B�B��B��B��B��B��B�oB�bB�\B�JB�7B�%B�B�B�B�B� B~�B}�B|�B{�Bz�By�Bx�Bw�Bv�Bu�Bt�Br�Bo�Bn�Bn�Bn�Bo�Bk�BhsBffBdZBcTBbNBcTBdZBcTBe`BjB}�B�1B�\B�bB��B��B�B�B�B��B�B�B�!B�'B�'B�!B�!B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�3B�?B�RB�^B�dB�dB�dB�jB�jB�qB�}BĜBǮBȴBȴBȴB��B��B��B��B��B��B�#B�/B�BB�HB�NB�fB�sB�sB�sB�sB�yB�yB�B�B�B�B�B��B��B��B��B	  B	B	B	
=B	VB	bB	uB	�B	�B	�B	$�B	&�B	'�B	'�B	'�B	,B	.B	33B	49B	5?B	6FB	8RB	9XB	9XB	;dB	?}B	A�B	D�B	E�B	F�B	H�B	H�B	I�B	J�B	K�B	K�B	L�B	N�B	VB	W
B	\)B	`BB	aHB	e`B	k�B	n�B	r�B	y�B	}�B	~�B	� B	� B	�B	�B	�B	�%B	�DB	�JB	�JB	�PB	�JB	�JB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�3B	�9B	�9B	�^B	�qB	�}B	��B	��B	B	ĜB	ŢB	ŢB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�;B	�;B	�;B	�NB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
�B
aB
7B
#�B
-�B
5tB
;�B
BB
I7B
O�B
UB
[#B
_�B
e�B
i_B
l�B
qB
r�B
t�B
{�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
i�B
j�B
k�B
m B
n B
qB
w;B
yKB
yDB
zNB
zIB
zKB
zKB
zIB
{SB
{UB
{QB
{NB
{RB
|YB
}^B
~fB
nB
lB
�rB
�qB
��B
��B
�CB
��B
�>Bu B�fB�}B��B��B�B� B��B�AB�B�B�VB�B%8B.nB.pB+[BF�BN/BZvBe�Bj�B�|B��B��B��B��B��B��B�B�B�&B�CB�aB�zB��B��B��B��B��B��B��B�B�B�B��B��B��B�CB�	B��B��B�xB�ZBz4Bh�B\�BP7BD�B@�B:�B4�B+`B!(B�B�B�;B�B�B��B�kB�B�kBrBd�B[�BTgBHB<�B1�B%NB�B�B
�$B
��B
׃B
�QB
�7B
�B
��B
�YB
�AB
�,B
�B
��B
��B
mB
^�B
SsB
CB
5�B
0�B
,�B
(wB
!JB
,B
�B
 �B	�'B	��B	זB	�WB	��B	��B	��B	�eB	�MB	�3B	�B	��B	��B	�B	zlB	xaB	r8B	iB	]�B	U�B	OnB	D*B	6�B	3�B	1�B	.�B	'B	0B	$B	B	�B��B�`B� B��B۽BدB՛B҇B�pB�RB�NB�KB�:B�0B�%B�B��B��B��B��B�cB�@B�1B�B��B��B��B��B��B��B~�B}�B|�B|�Bz�BxvBwrBvhBudBt^BsTBrRBqJBpGBoBBn8Bm3Bk&BhBgBgBgBhBc�B`�B^�B\�B[�BZ�B[�B\�B[�B]�Bb�BvlB��B��B��B�B�XB�zB��B�wB�nB�|B��B��B��B��B��B��B��B��B��B��B�zB�pB�gB�TB�@B�.B�B� B�B�2B�KB�TB�TB�VB�MB�TB�HB�SB�lB�iB�uB�~B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�$B�-B�+B�-B�7B�9B�HB�VB�hB�uBәBաBظB��B��B��B��B��B��B��B��B��B��B��B�B�B�B�0B�HB�ZB�gB�vB��B��B	�B	�B	�B	�B	�B	�B	$B	PB	[B	 cB	 eB	 bB	$yB	&�B	+�B	,�B	-�B	.�B	0�B	1�B	1�B	3�B	7�B	9�B	=B	>B	?B	A&B	A'B	B)B	C1B	D7B	D8B	E>B	GLB	NwB	OzB	T�B	X�B	Y�B	]�B	c�B	gB	kB	rKB	vaB	wkB	xqB	xnB	zzB	zzB	{}B	~�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�,B	�JB	�]B	�eB	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	� B	�7B	�9B	�@B	�DB	�IB	�LB	�NB	�QB	�TB	�eB	�mB	�zB	сB	҈B	ӎB	ԑB	ԓB	ԓB	ԔB	՚B	՚B	ףB	קB	קB	ںB	ۺB	ۻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�0B	�0B	�1B	�9B	�8B	�=B	�BB	�HB	�DB	�CB	�^B	�`B	�bB	�cB	�eB	�fG�O�B	��B
�B
�B
XB
&IB
-�B
4LB
:vB
A�B
HCB
M~B
S�B
X	B
]�B
a�B
d�B
isB
kbB
m	B
tPB
y811111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0), vertically averaged dS =-0.007(+/-0.001) in PSS-78.                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940272019060409402720190604094027  AO  ARCAADJP                                                                    20181121041157    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121041157  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121041157  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094027  IP                  G�O�G�O�G�O�                