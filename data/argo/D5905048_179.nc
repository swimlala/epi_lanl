CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-11-12T00:35:06Z creation;2017-11-12T00:35:10Z conversion to V3.1;2019-12-19T07:52:47Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171112003506  20200116221517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_179                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�4�Eg��1   @�4�    @4Pu��!��d��f�A�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�i�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @5�@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�=qA�=qA�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBX�B`�Bh�Bo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�{C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�{CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�@�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�@�D�g\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�n�A�hsA�bNA�^5A�\)A�XA�M�A�E�A�C�A�;dA��A��mAہA�JA�ȴA���A�|�A�  A؃A��A��`A��`A��TA��
A׶FA�v�A���AֶFA֓uA�O�AՅA��HAЋDA� �AʾwA��A�O�A�A�A��A�hsA�bA���A���A���A�5?A�VA�\)A���A�/A��jA���A��A��DA���A��;A�33A�ȴA�S�A��yA���A�bNA��A���A�?}A���A��yA��A�dZA��`A�5?A�XA���A�&�A�{A�C�A���A�"�A�K�A��!A���A�ƨA���A��`A���A�^5A�ȴA��+A�t�A�/A��A�bA��A���A�Q�A�VA���A�1A�
=A�v�A��HA��-A�S�A��A��TA��A}\)Az�!Aw33Au;dAt~�Aq�Ap9XAo�hAn��An�Am��Al5?Aj(�AgVAdn�Ab��AaA`$�A^��A]G�A\�/A\�A[��A[�AZ�/AZ�jAZ1AXJAV-AU?}AS�AQ�FAQ%AP��AO��AN5?AM+AJ�AIl�AG�
AF�DAD�AB�`A?/A<1A9�;A9+A7A4I�A3
=A2�9A1�mA1�7A1�A0ffA/��A/C�A.��A.I�A.{A-�hA,�jA,=qA+`BA*�+A)��A(��A(bNA($�A'�A'�
A&�A%7LA$��A#�mA#O�A!�mA ^5A7LA�wA�DA&�A33AVAA{Av�A�/A�Ap�A�RA1A�^A��A7LA��AAbA;dA~�A�;A
9XA��AI�A��A�AĜAJAt�An�A��A�+A�#AG�@��@�$�@���@�|�@���@�\)@��m@�&�@�@�~�@���@땁@�ff@��@�@��@��@���@��@���@��@�5?@�A�@���@ܣ�@�ƨ@ڰ!@�hs@��/@�j@��;@ׅ@�C�@֏\@Չ7@��m@���@�V@�Z@�1@�C�@Η�@���@�X@�33@��T@���@� �@Ǿw@Ǖ�@�"�@�V@�@�9X@�+@�ȴ@�@�&�@��u@��@�ƨ@�t�@�@�v�@�@���@��h@�X@�j@��@���@��#@�p�@��/@�j@��;@���@�dZ@��@���@�M�@��@��^@�/@��j@�b@���@��+@��@���@�hs@�V@���@��@�l�@��@�v�@�`B@��j@�j@�bN@�Z@���@��@��y@��!@�~�@��T@���@�x�@�`B@�?}@�hs@��7@�x�@�G�@�V@��/@�(�@��@�@��\@�ff@�=q@�$�@��7@�G�@��@��9@�(�@��
@�t�@��R@�$�@��h@�O�@��@���@��`@��D@�A�@�b@��;@���@�l�@�@��H@��@���@�n�@�=q@���@��@��T@��-@��h@��h@���@�`B@�/@�&�@��`@�j@�I�@�1@��w@���@�t�@�|�@���@�l�@�\)@��F@�Z@��u@��u@��@�/@��@��@��/@���@��D@�A�@��m@���@�C�@�"�@�
=@�@��y@�ff@�5?@���@�hs@��/@���@��D@�r�@�Z@�A�@� �@��@��@��
@��P@�dZ@�C�@��@��@�ȴ@�-@���@�p�@�?}@�V@��@���@���@�r�@� �@���@��F@��F@��F@���@�|�@�\)@�33@��!@�5?@��T@���@�?}@��`@�r�@�(�@���@�K�@�o@��y@�ȴ@���@�v�@��@�@��#@��-@���@�x�@�G�@��@���@��j@�r�@�A�@�1'@���@��w@�l�@�+@���@��@��H@�ȴ@��+@�n�@�^5@�-@��@�hs@�Ĝ@�Z@� �@�b@�1@�ƨ@���@�K�@�o@���@�V@�$�@��T@��h@���@�p�@�&�@�Ĝ@�r�@�A�@�  @��@��
@��w@��@���@�|�@�33@�o@��y@���@��!@�^5@��#@�p�@�V@��@��@��D@�j@�9X@�b@�w@�@~�R@~��@~V@~5?@}�@}��@}p�@}`B@}?}@}�@|�@|�j@|�@|��@|I�@{�m@{�F@{"�@z�H@z�!@z^5@zJ@y�@y�^@xĜ@xbN@x1'@xb@w�@w��@w�@v�@vff@v5?@v{@u�@u�T@u��@u�-@u�@u`B@u�@tI�@sƨ@sdZ@s33@s33@s"�@r�H@rn�@q�^@q&�@p��@p�9@pQ�@p  @o�w@o�P@o
=@n��@n5?@m�@mp�@l�/@l(�@kƨ@kC�@kC�@k33@j��@j��@j~�@j�@ihs@hĜ@h�@hQ�@g�@gl�@g;d@g�@f�R@f{@e�T@e/@d�/@dI�@d�@d1@c�m@c�F@c�@ct�@ct�@ct�@cC�@c@b��@a��@a��@ahs@ahs@`�9@` �@_�w@_\)@^��@^�+@]��@]�h@]?}@\��@\Z@\1@\1@[��@[�@[�@[S�@[o@Z�!@Y�@Yx�@Y�@X�9@XA�@W�P@W\)@W
=@V��@V�y@V�@Vv�@V$�@U�@UV@T�/@T�j@T�@Tj@S��@SS�@R�!@R��@R��@R��@R�\@R��@R~�@RJ@Q��@Qhs@Q�@P�`@P��@PĜ@P �@Ol�@O�@N�@N�+@M�@M��@MV@Lz�@L1@Kƨ@KS�@J��@J��@Jn�@JM�@I��@IG�@I�@H��@H�9@HA�@G�P@G;d@F�R@Fv�@F$�@E�T@E�h@D�@Dz�@D�@C��@C��@C�@CdZ@CS�@CC�@C"�@B��@Bn�@BJ@Ax�@A7L@A�@@�9@@r�@@Q�@@1'@@  @?��@?�P@?K�@>��@>@=�T@=��@=�h@=`B@<��@;S�@:�@:�\@:-@9�#@9��@9hs@9G�@9&�@8��@8��@8��@8�@8bN@8Q�@81'@7�@7l�@7�@6�y@6��@6ff@6{@5��@5�-@5�@5?}@4�@4�@4j@4Z@4(�@3�
@3��@3dZ@2�@2��@2�\@2^5@2=q@2-@1�#@1�^@1��@1��@1��@1x�@1x�@1hs@1hs@1�@0��@0bN@01'@0b@/�@/|�@.ȴ@.E�@-�T@-�-@-p�@-O�@-?}@-/@,��@,�@,�j@,�D@,z�@,j@,j@,j@,9X@+��@+�
@+�F@+�@+33@*�!@*n�@*-@)�@)��@)hs@)G�@)G�@)&�@(Ĝ@(�u@(�u@(�u@(r�@( �@'�;@'��@'K�@'�@&�@&V@%�@%��@%`B@%/@%V@$�/@$�/@$�j@$�D@$j@$Z@$I�@$(�@$1@#�m@#��@#t�@#33@"�H@"��@"��@"^5@"J@!�@!�#@!��@!��@!G�@ �`@ Ĝ@ ��@ �@ A�@ 1'@ A�@  �@ 1'@ b@ b@�@�;@l�@+@�@�@��@��@��@�@��@V@5?@@p�@?}@�@��@�/@z�@I�@9X@�@�m@��@t�@S�@"�@�@M�@=q@-@�@J@J@��@�^@7L@��@�u@Q�@  @�;@�w@|�@|�@\)@;d@�@
=@�@ȴ@�R@��@��@ff@V@$�@��@�@`B@O�@�@�@�@z�@j@Z@9X@1@��@�m@ƨ@��@t�@33@o@�@��@~�@^5@=q@�@J@J@��@��@�7@x�@7L@��@�u@bN@A�@1'@1'@1'@ �@�@|�@
=@�@ȴ@��@v�@ff@�@�-@/@��@�/@�D@j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�n�A�hsA�bNA�^5A�\)A�XA�M�A�E�A�C�A�;dA��A��mAہA�JA�ȴA���A�|�A�  A؃A��A��`A��`A��TA��
A׶FA�v�A���AֶFA֓uA�O�AՅA��HAЋDA� �AʾwA��A�O�A�A�A��A�hsA�bA���A���A���A�5?A�VA�\)A���A�/A��jA���A��A��DA���A��;A�33A�ȴA�S�A��yA���A�bNA��A���A�?}A���A��yA��A�dZA��`A�5?A�XA���A�&�A�{A�C�A���A�"�A�K�A��!A���A�ƨA���A��`A���A�^5A�ȴA��+A�t�A�/A��A�bA��A���A�Q�A�VA���A�1A�
=A�v�A��HA��-A�S�A��A��TA��A}\)Az�!Aw33Au;dAt~�Aq�Ap9XAo�hAn��An�Am��Al5?Aj(�AgVAdn�Ab��AaA`$�A^��A]G�A\�/A\�A[��A[�AZ�/AZ�jAZ1AXJAV-AU?}AS�AQ�FAQ%AP��AO��AN5?AM+AJ�AIl�AG�
AF�DAD�AB�`A?/A<1A9�;A9+A7A4I�A3
=A2�9A1�mA1�7A1�A0ffA/��A/C�A.��A.I�A.{A-�hA,�jA,=qA+`BA*�+A)��A(��A(bNA($�A'�A'�
A&�A%7LA$��A#�mA#O�A!�mA ^5A7LA�wA�DA&�A33AVAA{Av�A�/A�Ap�A�RA1A�^A��A7LA��AAbA;dA~�A�;A
9XA��AI�A��A�AĜAJAt�An�A��A�+A�#AG�@��@�$�@���@�|�@���@�\)@��m@�&�@�@�~�@���@땁@�ff@��@�@��@��@���@��@���@��@�5?@�A�@���@ܣ�@�ƨ@ڰ!@�hs@��/@�j@��;@ׅ@�C�@֏\@Չ7@��m@���@�V@�Z@�1@�C�@Η�@���@�X@�33@��T@���@� �@Ǿw@Ǖ�@�"�@�V@�@�9X@�+@�ȴ@�@�&�@��u@��@�ƨ@�t�@�@�v�@�@���@��h@�X@�j@��@���@��#@�p�@��/@�j@��;@���@�dZ@��@���@�M�@��@��^@�/@��j@�b@���@��+@��@���@�hs@�V@���@��@�l�@��@�v�@�`B@��j@�j@�bN@�Z@���@��@��y@��!@�~�@��T@���@�x�@�`B@�?}@�hs@��7@�x�@�G�@�V@��/@�(�@��@�@��\@�ff@�=q@�$�@��7@�G�@��@��9@�(�@��
@�t�@��R@�$�@��h@�O�@��@���@��`@��D@�A�@�b@��;@���@�l�@�@��H@��@���@�n�@�=q@���@��@��T@��-@��h@��h@���@�`B@�/@�&�@��`@�j@�I�@�1@��w@���@�t�@�|�@���@�l�@�\)@��F@�Z@��u@��u@��@�/@��@��@��/@���@��D@�A�@��m@���@�C�@�"�@�
=@�@��y@�ff@�5?@���@�hs@��/@���@��D@�r�@�Z@�A�@� �@��@��@��
@��P@�dZ@�C�@��@��@�ȴ@�-@���@�p�@�?}@�V@��@���@���@�r�@� �@���@��F@��F@��F@���@�|�@�\)@�33@��!@�5?@��T@���@�?}@��`@�r�@�(�@���@�K�@�o@��y@�ȴ@���@�v�@��@�@��#@��-@���@�x�@�G�@��@���@��j@�r�@�A�@�1'@���@��w@�l�@�+@���@��@��H@�ȴ@��+@�n�@�^5@�-@��@�hs@�Ĝ@�Z@� �@�b@�1@�ƨ@���@�K�@�o@���@�V@�$�@��T@��h@���@�p�@�&�@�Ĝ@�r�@�A�@�  @��@��
@��w@��@���@�|�@�33@�o@��y@���@��!@�^5@��#@�p�@�V@��@��@��D@�j@�9X@�b@�w@�@~�R@~��@~V@~5?@}�@}��@}p�@}`B@}?}@}�@|�@|�j@|�@|��@|I�@{�m@{�F@{"�@z�H@z�!@z^5@zJ@y�@y�^@xĜ@xbN@x1'@xb@w�@w��@w�@v�@vff@v5?@v{@u�@u�T@u��@u�-@u�@u`B@u�@tI�@sƨ@sdZ@s33@s33@s"�@r�H@rn�@q�^@q&�@p��@p�9@pQ�@p  @o�w@o�P@o
=@n��@n5?@m�@mp�@l�/@l(�@kƨ@kC�@kC�@k33@j��@j��@j~�@j�@ihs@hĜ@h�@hQ�@g�@gl�@g;d@g�@f�R@f{@e�T@e/@d�/@dI�@d�@d1@c�m@c�F@c�@ct�@ct�@ct�@cC�@c@b��@a��@a��@ahs@ahs@`�9@` �@_�w@_\)@^��@^�+@]��@]�h@]?}@\��@\Z@\1@\1@[��@[�@[�@[S�@[o@Z�!@Y�@Yx�@Y�@X�9@XA�@W�P@W\)@W
=@V��@V�y@V�@Vv�@V$�@U�@UV@T�/@T�j@T�@Tj@S��@SS�@R�!@R��@R��@R��@R�\@R��@R~�@RJ@Q��@Qhs@Q�@P�`@P��@PĜ@P �@Ol�@O�@N�@N�+@M�@M��@MV@Lz�@L1@Kƨ@KS�@J��@J��@Jn�@JM�@I��@IG�@I�@H��@H�9@HA�@G�P@G;d@F�R@Fv�@F$�@E�T@E�h@D�@Dz�@D�@C��@C��@C�@CdZ@CS�@CC�@C"�@B��@Bn�@BJ@Ax�@A7L@A�@@�9@@r�@@Q�@@1'@@  @?��@?�P@?K�@>��@>@=�T@=��@=�h@=`B@<��@;S�@:�@:�\@:-@9�#@9��@9hs@9G�@9&�@8��@8��@8��@8�@8bN@8Q�@81'@7�@7l�@7�@6�y@6��@6ff@6{@5��@5�-@5�@5?}@4�@4�@4j@4Z@4(�@3�
@3��@3dZ@2�@2��@2�\@2^5@2=q@2-@1�#@1�^@1��@1��@1��@1x�@1x�@1hs@1hs@1�@0��@0bN@01'@0b@/�@/|�@.ȴ@.E�@-�T@-�-@-p�@-O�@-?}@-/@,��@,�@,�j@,�D@,z�@,j@,j@,j@,9X@+��@+�
@+�F@+�@+33@*�!@*n�@*-@)�@)��@)hs@)G�@)G�@)&�@(Ĝ@(�u@(�u@(�u@(r�@( �@'�;@'��@'K�@'�@&�@&V@%�@%��@%`B@%/@%V@$�/@$�/@$�j@$�D@$j@$Z@$I�@$(�@$1@#�m@#��@#t�@#33@"�H@"��@"��@"^5@"J@!�@!�#@!��@!��@!G�@ �`@ Ĝ@ ��@ �@ A�@ 1'@ A�@  �@ 1'@ b@ b@�@�;@l�@+@�@�@��@��@��@�@��@V@5?@@p�@?}@�@��@�/@z�@I�@9X@�@�m@��@t�@S�@"�@�@M�@=q@-@�@J@J@��@�^@7L@��@�u@Q�@  @�;@�w@|�@|�@\)@;d@�@
=@�@ȴ@�R@��@��@ff@V@$�@��@�@`B@O�@�@�@�@z�@j@Z@9X@1@��@�m@ƨ@��@t�@33@o@�@��@~�@^5@=q@�@J@J@��@��@�7@x�@7L@��@�u@bN@A�@1'@1'@1'@ �@�@|�@
=@�@ȴ@��@v�@ff@�@�-@/@��@�/@�D@j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B�{B��B��B��B��B��B��B��B��B��B�B�^BɺB�5B��B��B�B�BBBBBBBBbB�B!�B!�BuB{B"�B �B>wB=qB7LBB�B[#Bq�B`BB[#Bt�Bl�Bk�BjB�B�+B�1B�7B��B��B��B�hB�\B�\B�B~�B�B{�Bw�Bz�BjBE�B5?B5?B0!B)�B�BVB	7BB��B�B�HBĜB�dB�B��B�\B�By�Bu�Bp�BffBT�BA�B,B/B.B,B'�B"�B�B�BPBB
�B
�fB
��B
�RB
�\B
w�B
Q�B
9XB
(�B
uB

=B
%B	��B	�B	�B	�mB	�NB	�B	��B	�wB	�B	��B	�uB	�PB	�1B	�B	x�B	w�B	v�B	o�B	l�B	jB	hsB	bNB	VB	K�B	F�B	@�B	6FB	5?B	49B	/B	&�B	$�B	�B	�B	PB	B��B�B�BĜB�dB�RB�B��B��B��B��B��B��B��B��B��B��B�{B�{B�oB�PB�PB�+B�B�B�B�B�B�B�By�Bq�Br�Bq�Bm�BhsB`BBaHB\)B[#B]/BgmBk�BjBffBk�BgmBgmBffBdZBcTBcTBbNB^5BYBR�BS�BS�BS�BP�BK�BJ�BS�BR�BQ�BT�BP�BN�BL�BJ�BI�BN�BM�BH�BM�BL�BK�BG�BH�BC�BI�BO�BR�BP�BT�BW
B\)B\)B[#BZB^5BaHB_;B\)B`BB^5B]/Be`BgmBgmBiyBm�Bo�Bo�Bp�Bp�Bo�Bn�Bm�Bp�Bp�By�B|�B{�B}�B}�B� B{�B�B�1B�DB�VB�bB�VB�VB�\B�\B�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�?B�dB�jB�}B��BŢBŢBƨBǮB��B��B��B��B��B��B�
B�HB�`B�B�B�B�B��B��B��B��B��B	%B	\B	{B	{B	uB	�B	�B	�B	 �B	�B	$�B	&�B	)�B	-B	/B	2-B	5?B	5?B	6FB	5?B	49B	6FB	7LB	:^B	<jB	=qB	>wB	=qB	A�B	C�B	D�B	G�B	K�B	M�B	P�B	VB	[#B	bNB	cTB	dZB	e`B	e`B	hsB	k�B	n�B	q�B	r�B	s�B	x�B	z�B	y�B	{�B	~�B	�B	�B	�B	�B	�+B	�7B	�7B	�7B	�DB	�JB	�JB	�PB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�9B	�FB	�FB	�FB	�LB	�XB	�dB	�qB	�}B	��B	��B	��B	�wB	��B	B	B	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�5B	�;B	�BB	�HB	�TB	�`B	�fB	�fB	�fB	�`B	�`B	�`B	�TB	�`B	�fB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B	��B
  B
  B
  B	��B	��B	��B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B

=B

=B

=B

=B

=B
DB
PB
bB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
 �B
 �B
 �B
�B
�B
�B
�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
 �B
 �B
�B
 �B
"�B
#�B
#�B
#�B
"�B
"�B
!�B
!�B
#�B
$�B
#�B
$�B
%�B
%�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
'�B
(�B
)�B
)�B
(�B
)�B
)�B
(�B
'�B
(�B
)�B
)�B
+B
)�B
+B
+B
+B
+B
-B
+B
-B
-B
.B
/B
/B
/B
/B
0!B
0!B
0!B
/B
.B
.B
.B
/B
0!B
0!B
.B
.B
/B
0!B
0!B
0!B
/B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
49B
49B
33B
33B
2-B
33B
33B
49B
49B
33B
5?B
5?B
6FB
6FB
6FB
5?B
5?B
49B
5?B
6FB
6FB
6FB
5?B
49B
49B
5?B
8RB
9XB
9XB
:^B
:^B
:^B
9XB
9XB
:^B
:^B
:^B
;dB
:^B
9XB
8RB
;dB
<jB
<jB
;dB
<jB
;dB
<jB
>wB
>wB
>wB
>wB
@�B
@�B
@�B
?}B
?}B
A�B
A�B
@�B
?}B
?}B
A�B
@�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
F�B
E�B
G�B
G�B
G�B
H�B
H�B
I�B
H�B
H�B
I�B
H�B
H�B
H�B
J�B
J�B
J�B
I�B
H�B
F�B
J�B
K�B
K�B
K�B
M�B
M�B
N�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
Q�B
R�B
R�B
R�B
S�B
S�B
R�B
R�B
R�B
R�B
T�B
T�B
VB
VB
VB
T�B
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
VB
VB
W
B
XB
W
B
W
B
VB
VB
XB
XB
YB
ZB
ZB
[#B
[#B
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
[#B
[#B
\)B
\)B
[#B
[#B
[#B
\)B
]/B
\)B
]/B
]/B
^5B
^5B
^5B
]/B
^5B
_;B
_;B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
^5B
_;B
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
bNB
cTB
cTB
cTB
dZB
dZB
cTB
dZB
e`B
e`B
e`B
dZB
dZB
dZB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
ffB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
gmB
hsB
gmB
hsB
iyB
iyB
jB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
jB
k�B
jB
l�B
l�B
l�B
l�B
m�B
l�B
k�B
k�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
u�B
t�B
u�B
u�B
v�B
w�B
v�B
v�B
v�B
v�B
w�B
v�B
x�B
x�B
x�B
y�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B�	B�VB��B�dBʦB߾B��B��B��B��B�B3B3BMB{B�BB B/B#B$�B�B�B(>B'�BBuBA�B=�BH�B^�BuZBe`B_�Bw�Bn�BnIBm�B�'B��B�RB��B��B��B�B�B��B�HB�?B� B�-B~BBx�B|�BnIBK^B8�B7LB1�B+kB �BB
�B�B�JB�B��BɆB��B��B��B��B�EB{�BvzBq�BhsBXyBE�B.}B/OB.cB,qB(�B#�B�B�B�B-B
�MB
�XB
�,B
�jB
��B
|PB
WsB
=�B
,qB
?B
�B
�B	�$B	�B	�]B	�>B	� B	یB	��B	�oB	��B	��B	��B	�(B	�lB	��B	z�B	x�B	wLB	p�B	m]B	kB	i*B	c�B	X�B	NB	HKB	B�B	8lB	6+B	5B	0oB	(�B	&�B	qB	?B	vB	B�VB�BݘB�fB�B��B��B�hB�OB�bB��B�CB�~B��B�YB�YB�YB�MB��B�@B�pB�"B�fB�YB�-B�AB��B��B�uB��B{JBs�Bs�Br�Bn�BjBbhBb�B^B\�B^�Bg�BlWBlqBh�Bm�Bi_Bh�Bg�Be`BdZBc�Bb�B_!BZQBT�BUgBUMBUBR:BNBL�BT�BTBR�BU�BR BO�BN<BL0BK)BO�BN�BJ�BN�BNBMBI�BJXBE�BKxBQBS�BRBVBW�B\�B\�B[�B[=B^�Ba�B`B]�Ba-B_�B^�Bf2Bh>Bh>BjKBm�BpBp!Bp�BqBpUBo�Bn�Bq�Bq�Bz^B}qB|�B~�B~�B��B}qB��B��B��B��B��B��B�B��B��B�2B�	B�=B�OB�4B�@B�LB�8B�RB�sB�QB�CB�]B�]B��B��B��B��B��B��B��B�B��B��B��B�B�B�B�<B�\B�NBӏB��B�B��B��B��B�B�oB�B�<B�VB��B��B	�B	�B	�B	�B	�B	B	!B	�B	 �B	 \B	%B	'8B	*B	-)B	/B	2GB	5ZB	5tB	6zB	5�B	4�B	6�B	7�B	:�B	<�B	=�B	>�B	>B	A�B	C�B	EB	H1B	LB	N<B	QhB	VmB	[�B	b�B	c�B	d�B	ezB	e�B	h�B	k�B	n�B	q�B	r�B	tB	y	B	z�B	zB	|B	.B	�;B	�3B	�9B	�mB	�EB	�RB	�RB	�lB	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�-B	�MB	�TB	�`B	�`B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�B	� B	� B	�B	�B	�B	�4B	�&B	�:B	�bB	�FB	�9B	�EB	�QB	�CB	�jB	�pB	�vB	�B	�B	�B	�fB	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�	B	�B	�B	�0B	�6B	�(B	�.B
 B
 B
 B	�.B
 B
 B
 4B	�HB	�cB	�cB
uB
GB
9B
SB
MB
SB
�B
SB
�B
tB
zB
�B
fB

=B

�B

�B

�B

�B
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
 �B
�B
B
�B
 �B
 �B
 �B
�B
�B
B
�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
 �B
 �B
 B
 �B
#B
$B
#�B
#�B
#B
#B
"B
"B
$B
$�B
$&B
%B
%�B
%�B
%B
&B
&2B
%�B
&B
&B
&B
($B
)*B
*B
*B
)B
*B
*B
)*B
(>B
)*B
*0B
*B
+6B
*0B
+6B
+6B
+6B
+6B
-)B
+QB
-)B
-]B
./B
/5B
/5B
/5B
/5B
0!B
0!B
0!B
/5B
./B
.IB
.cB
/5B
0;B
0;B
.}B
.IB
/iB
0oB
0UB
0oB
/iB
1AB
1[B
1vB
1[B
1AB
2-B
2GB
3MB
49B
4nB
3MB
3�B
2�B
3�B
3�B
4nB
4nB
3hB
5tB
5tB
6FB
6`B
6zB
5�B
5tB
4nB
5tB
6`B
6`B
6`B
5ZB
4nB
4�B
5�B
8RB
9XB
9rB
:^B
:xB
:�B
9�B
9�B
:xB
:xB
:�B
;B
:xB
9�B
8�B
;B
<�B
<�B
;�B
<�B
;�B
<�B
>�B
>�B
>�B
>�B
@�B
@�B
@�B
?�B
?�B
A�B
A�B
@�B
?�B
?�B
A�B
@�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
D�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
E�B
F�B
E�B
G�B
G�B
G�B
H�B
H�B
I�B
H�B
H�B
I�B
H�B
IB
H�B
J�B
J�B
J�B
J	B
IB
G+B
KB
K�B
K�B
K�B
M�B
M�B
OB
PB
PB
QB
QB
QB
R B
Q�B
QB
Q B
Q4B
QB
RB
RB
RB
RB
RB
S&B
S&B
RB
S&B
SB
SB
S�B
TB
SB
S&B
S&B
S@B
UB
UB
V9B
V9B
VB
U2B
VB
W
B
W$B
W
B
W?B
W
B
W
B
W
B
VB
V9B
W?B
X+B
W?B
W?B
V9B
VSB
XEB
XEB
YKB
ZQB
Z7B
[#B
[WB
ZQB
[#B
[=B
[=B
\CB
\)B
\CB
\CB
[WB
[WB
\CB
\CB
[=B
[=B
[qB
\CB
]dB
\]B
]IB
]IB
^5B
^OB
^jB
]dB
^OB
_;B
_VB
^jB
^OB
^jB
^OB
^OB
_VB
_pB
^�B
_VB
`\B
`vB
abB
bhB
bhB
bNB
bhB
bhB
bhB
cnB
cTB
c�B
cnB
cnB
b�B
cnB
cnB
cnB
dZB
dtB
cnB
dtB
ezB
e`B
e`B
dtB
dtB
dtB
ffB
f�B
f�B
f�B
g�B
gmB
gmB
g�B
g�B
gmB
g�B
f�B
f�B
g�B
h�B
hsB
h�B
hsB
hsB
h�B
h�B
g�B
h�B
g�B
h�B
i�B
i�B
j�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
j�B
k�B
j�B
l�B
l�B
l�B
l�B
m�B
l�B
k�B
k�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
n�B
o�B
p�B
p�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
v�B
v�B
v�B
u�B
t�B
u�B
vB
v�B
w�B
v�B
v�B
v�B
wB
w�B
v�B
y	B
y	B
x�B
y�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.07(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711160039302017111600393020171116003930201806221321522018062213215220180622132152201804050724482018040507244820180405072448  JA  ARFMdecpA19c                                                                20171112093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171112003506  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171112003508  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171112003509  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171112003510  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171112003510  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171112003510  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171112003510  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171112003510  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171112003510                      G�O�G�O�G�O�                JA  ARUP                                                                        20171112005523                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171112153317  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20171115153930  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171115153930  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222448  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042152  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221517                      G�O�G�O�G�O�                