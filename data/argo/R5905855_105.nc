CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:29:26Z creation;2022-06-04T19:29:26Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192926  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               iA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @٭[��r1   @٭\f�	@+��n���d, ě��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @333@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  AᙚA�  B   B  B  B  B   B(  B0  B8  B@  BH  BQ33BVffB`��Bg��Bp  Bx  B�ffB�ffB���B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B���B���B���C  C  C  C  C
  C  C  C  C  C  C  C  C33C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Db��Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @.{@z�H@�p�@�p�A�RA>�RA^�RA~�RA��\A�\)A�\)A�\)A�\)A���A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BP�GBVzB`z�BgG�Bo�Bw�B�=pB�=pB�p�B��
B��
B��
B��
B���B��
B��
B��
B��
B��
B��
B��
B�=pB��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B�=pB��B��
B���B���B���C�C�C�C�C	�C�C�C�C�C�C�C�C�C��C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CFCG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db�{Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��>D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD� �D�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD� �D�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��>D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD� �D�@�D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��PA���A� �A���A��ZA��A�oA�YA�1A��A�	7A�
�A�	�A�	�A�
	A�DA��A�oA���A��A��2A��A���A҃�A���AѰUAѳ�AќxAύPA�$tA�}�A��A��Aá�A«A�dZA��A�_;A�gA���A���A��A��A��A�.A�q�A�rGA��|A���A�$�A�OBA�A���A�{A���A��%A��CA�7�A��A��lA�(�A���A��A�%A���A���A��_A���A��A�HA��aA���A���A�]dA� Az�4At4ApIRAl�Ai�tAg��Ad�Aa�IA[�AX��AT��AQ�5ANoAG�AC$tAA��A>=�A<J�A;A:�	A:U�A7�CA5A A3�A2+A0�A-�	A-	�A,خA,�A,*�A*�A&�oA%�A$E�A"˒A"8�A!XA �3A 6A�dA��A��A��A�vA�|A�"A�AoA�A�AĜA��A33A�An/AƨAD�Ap;AAiDA�Ak�A��AU�A�jA�[Aw�A�+AVA�yA�FA?}A��A��AA\)A�SAk�A4nA�A��A!-A�QA�A
�A
�@A
MA
,=A
�A	�A	�zA	s�A�A�HA�gAϫA�A��A�MAn/AJ�A(�A��A�-A�hA��A�YA�8A^5A��A�AںA��AFtA�A�FA�xAB[A�-A�A ��@���@��$@�5�@��^@��4@�_@��M@�c�@���@��@���@���@�u%@��j@�O@�F�@�Mj@��@�^5@��@��b@�4@��@���@�K^@�=@���@��@��s@��@�k@�u�@�:@�x�@��@�Xy@�d�@�S&@��@��@�k�@��@��U@�_@��@�w@�@O@��)@�s�@��@�[W@�q@���@�s@�K�@��	@���@��@��@�Y@�w@��P@�$@�M@��v@��r@�S&@��@ܬ@�x@۲-@�c�@��@��`@�YK@ٔ�@�2a@�oi@בh@���@�g8@Յ@Ԝx@��@��@Ҋr@�1�@��+@Ѣ�@��@Ю}@�N�@�6@�	�@�^�@��@ΰ!@��@̺�@̤�@�h
@�7�@��@ˠ'@�b�@�"�@���@ʫ6@�:*@�f�@��)@�Ta@��+@�y�@��@�'R@ŷ�@�=@ġb@�N�@�u@é*@�n/@��@�'R@��~@�g�@�`B@�S�@��@�r�@�"h@���@�t�@�6z@�'�@��@�z@���@��@��)@��+@�R�@�#:@��@���@��s@�4n@��f@�/@� i@���@��!@��j@���@�}V@��K@���@�X@��D@�7@���@�N<@�4@��@���@���@�:*@��0@�rG@�Mj@�9�@���@�5?@���@�]�@��@��y@���@�c�@�@���@�o@�4n@��;@�f�@��@�M@�Y�@��"@��1@�:*@��@��.@��@�j@��@��O@�U2@�+k@���@��s@��@��{@��@�M@�  @�>�@�	l@���@�|�@�U2@�'R@���@��@��[@��@�d�@�-@�4@� �@��t@��@���@���@���@��@���@�B[@��@���@��@�!�@���@�Vm@�V@��H@���@�@��P@��@��+@�1'@��a@��'@��f@��@��@�Xy@�B[@�4@��g@��	@�A�@�#�@�@��]@��$@���@��D@�1'@�j�@��@�xl@�H@��+@��^@���@�p�@��@���@��+@�p;@�8�@��N@�c�@�Dg@��@��y@��@�=q@�7@�
�@���@��#@���@��$@�8@��@��@���@�M�@�@��@�ƨ@��4@�<6@�%F@��@��]@�y>@�:*@��}@��@@�u�@�o @�IR@�V@��@���@���@�tT@�oi@�@�@��j@��@��t@��q@���@�x@�IR@�'�@�
=@��,@�z�@�-�@���@�s@�^�@�&�@���@�H�@��@��P@�Dg@���@��E@��!@�l"@�G@8@~�@~��@~8�@}��@}`B@|�[@|��@|!@{33@zM�@y�t@yY�@y \@x�[@xu�@x!@w�V@wU�@w&@v��@vGE@u�@uQ�@t�j@t��@t�D@s�Q@s{J@sK�@s+@r��@r��@r��@r@�@q�~@p�.@p�@oX�@n�!@n6�@mԕ@m@@l�@k��@k)_@j�@j��@j3�@io @h��@g�@gY@f��@f҉@f;�@e�-@e��@e�@c�}@c�V@c��@cqv@c6z@bȴ@b��@b0U@a��@`�$@`1@_s@_&@^�s@^�A@^e@]��@]�9@]��@]��@]F@](�@]�@]�@]�@]@\��@\w�@[��@[�P@[X�@Z��@Z~�@ZW�@Ze@Y��@Y8�@X��@Xe�@X�@Wƨ@WF�@V��@U�@U�h@Uc�@UDg@T��@T�4@T_@T�@S�*@S@O@R�@R��@R($@QL�@P��@Pm�@P_@P  @O��@O4�@N�@NJ�@N!�@M�@M�^@Mm]@M/@L�E@L�_@Lm�@L,=@K��@LG@L	�@L�@L!@LG@K�A@K�@K�+@K�@K�@K˒@Ke�@J}V@J�@I�>@I�z@I��@I��@IVm@Hu�@HFt@H:�@G�@G�P@F}V@F_@E��@E \@D�?@D��@DC-@C�@B��@B��@Bh
@B�@A@AB�@@M@?�P@>��@>�r@>GE@>{@>�@=�j@=��@=Vm@=G�@<�@<!@;�@;��@;RT@;)_@:�2@:��@:V@9��@9N<@95�@9�@9�@8ѷ@8��@8A�@7��@7��@7>�@6�@6B[@5��@5��@5<6@4��@4�@4�O@4H@3��@3Mj@3o@2��@2;�@2�@1�N@1^�@0��@0�e@0�D@0I�@/�w@/J#@/(@.��@.!�@-�@-B�@,�@,w�@,*�@+�;@+��@+��@+�:@+��@+|�@+j�@+J#@+"�@*�]@*��@*s�@*?@)��@)�@)��@)^�@)2a@(�)@(!@'� @'�*@'�$@'Z�@&��@&�\@&Z�@%�j@%�@%e,@%Y�@%@$��@$]d@#�@#�@#�f@#iD@#;d@#$t@#�@"҉@"�@"d�@"8�@"�@!�)@![W@!�@ �E@ w�@ �@�[@;d@ߤ@��@�+@i�@h
@a|@5?@�^@o @4@�f@��@l"@S�@K^@6@b@�W@�q@l�@J#@H�@�@�c@�X@}V@5?@�@�D@ԕ@�-@�@A @�@�)@��@��@�)@j@1'@��@ƨ@�q@P�@�@��@}V@Ov@_@�@�j@��@��@�X@��@a�@%@��@?�@�@�6@��@\)@33@��@�L@^5@J�@-@��@�-@s�@Vm@:�@@��@l"@:�@@�A@�P@U�@.I@�@ں@��@�!@�r@V@;�@.�@��@��@`B@Dg@=�@#�@��@�e@tT@Xy@A�@/�@~@x@�@  @�+@�w@o�@6z@"�@@
�2@
�r@
�@
��@
��@
�B@
�!@
R�@
)�@	��@	��@	�h@	��@	k�@	-w@֡@oi@*�@@�@��@��@��@~�@X�@U�@n/@dZ@W?@9�@1�@�@��@�+@xl@M�@^5@=q@_@�>@��@@Vm@&�@;@��@��@�I@�@]d@M@A�@4n@M@�@�}@�@�0@��@�[@��@��@��@��@�@s@;d@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��PA���A� �A���A��ZA��A�oA�YA�1A��A�	7A�
�A�	�A�	�A�
	A�DA��A�oA���A��A��2A��A���A҃�A���AѰUAѳ�AќxAύPA�$tA�}�A��A��Aá�A«A�dZA��A�_;A�gA���A���A��A��A��A�.A�q�A�rGA��|A���A�$�A�OBA�A���A�{A���A��%A��CA�7�A��A��lA�(�A���A��A�%A���A���A��_A���A��A�HA��aA���A���A�]dA� Az�4At4ApIRAl�Ai�tAg��Ad�Aa�IA[�AX��AT��AQ�5ANoAG�AC$tAA��A>=�A<J�A;A:�	A:U�A7�CA5A A3�A2+A0�A-�	A-	�A,خA,�A,*�A*�A&�oA%�A$E�A"˒A"8�A!XA �3A 6A�dA��A��A��A�vA�|A�"A�AoA�A�AĜA��A33A�An/AƨAD�Ap;AAiDA�Ak�A��AU�A�jA�[Aw�A�+AVA�yA�FA?}A��A��AA\)A�SAk�A4nA�A��A!-A�QA�A
�A
�@A
MA
,=A
�A	�A	�zA	s�A�A�HA�gAϫA�A��A�MAn/AJ�A(�A��A�-A�hA��A�YA�8A^5A��A�AںA��AFtA�A�FA�xAB[A�-A�A ��@���@��$@�5�@��^@��4@�_@��M@�c�@���@��@���@���@�u%@��j@�O@�F�@�Mj@��@�^5@��@��b@�4@��@���@�K^@�=@���@��@��s@��@�k@�u�@�:@�x�@��@�Xy@�d�@�S&@��@��@�k�@��@��U@�_@��@�w@�@O@��)@�s�@��@�[W@�q@���@�s@�K�@��	@���@��@��@�Y@�w@��P@�$@�M@��v@��r@�S&@��@ܬ@�x@۲-@�c�@��@��`@�YK@ٔ�@�2a@�oi@בh@���@�g8@Յ@Ԝx@��@��@Ҋr@�1�@��+@Ѣ�@��@Ю}@�N�@�6@�	�@�^�@��@ΰ!@��@̺�@̤�@�h
@�7�@��@ˠ'@�b�@�"�@���@ʫ6@�:*@�f�@��)@�Ta@��+@�y�@��@�'R@ŷ�@�=@ġb@�N�@�u@é*@�n/@��@�'R@��~@�g�@�`B@�S�@��@�r�@�"h@���@�t�@�6z@�'�@��@�z@���@��@��)@��+@�R�@�#:@��@���@��s@�4n@��f@�/@� i@���@��!@��j@���@�}V@��K@���@�X@��D@�7@���@�N<@�4@��@���@���@�:*@��0@�rG@�Mj@�9�@���@�5?@���@�]�@��@��y@���@�c�@�@���@�o@�4n@��;@�f�@��@�M@�Y�@��"@��1@�:*@��@��.@��@�j@��@��O@�U2@�+k@���@��s@��@��{@��@�M@�  @�>�@�	l@���@�|�@�U2@�'R@���@��@��[@��@�d�@�-@�4@� �@��t@��@���@���@���@��@���@�B[@��@���@��@�!�@���@�Vm@�V@��H@���@�@��P@��@��+@�1'@��a@��'@��f@��@��@�Xy@�B[@�4@��g@��	@�A�@�#�@�@��]@��$@���@��D@�1'@�j�@��@�xl@�H@��+@��^@���@�p�@��@���@��+@�p;@�8�@��N@�c�@�Dg@��@��y@��@�=q@�7@�
�@���@��#@���@��$@�8@��@��@���@�M�@�@��@�ƨ@��4@�<6@�%F@��@��]@�y>@�:*@��}@��@@�u�@�o @�IR@�V@��@���@���@�tT@�oi@�@�@��j@��@��t@��q@���@�x@�IR@�'�@�
=@��,@�z�@�-�@���@�s@�^�@�&�@���@�H�@��@��P@�Dg@���@��E@��!@�l"@�G@8@~�@~��@~8�@}��@}`B@|�[@|��@|!@{33@zM�@y�t@yY�@y \@x�[@xu�@x!@w�V@wU�@w&@v��@vGE@u�@uQ�@t�j@t��@t�D@s�Q@s{J@sK�@s+@r��@r��@r��@r@�@q�~@p�.@p�@oX�@n�!@n6�@mԕ@m@@l�@k��@k)_@j�@j��@j3�@io @h��@g�@gY@f��@f҉@f;�@e�-@e��@e�@c�}@c�V@c��@cqv@c6z@bȴ@b��@b0U@a��@`�$@`1@_s@_&@^�s@^�A@^e@]��@]�9@]��@]��@]F@](�@]�@]�@]�@]@\��@\w�@[��@[�P@[X�@Z��@Z~�@ZW�@Ze@Y��@Y8�@X��@Xe�@X�@Wƨ@WF�@V��@U�@U�h@Uc�@UDg@T��@T�4@T_@T�@S�*@S@O@R�@R��@R($@QL�@P��@Pm�@P_@P  @O��@O4�@N�@NJ�@N!�@M�@M�^@Mm]@M/@L�E@L�_@Lm�@L,=@K��@LG@L	�@L�@L!@LG@K�A@K�@K�+@K�@K�@K˒@Ke�@J}V@J�@I�>@I�z@I��@I��@IVm@Hu�@HFt@H:�@G�@G�P@F}V@F_@E��@E \@D�?@D��@DC-@C�@B��@B��@Bh
@B�@A@AB�@@M@?�P@>��@>�r@>GE@>{@>�@=�j@=��@=Vm@=G�@<�@<!@;�@;��@;RT@;)_@:�2@:��@:V@9��@9N<@95�@9�@9�@8ѷ@8��@8A�@7��@7��@7>�@6�@6B[@5��@5��@5<6@4��@4�@4�O@4H@3��@3Mj@3o@2��@2;�@2�@1�N@1^�@0��@0�e@0�D@0I�@/�w@/J#@/(@.��@.!�@-�@-B�@,�@,w�@,*�@+�;@+��@+��@+�:@+��@+|�@+j�@+J#@+"�@*�]@*��@*s�@*?@)��@)�@)��@)^�@)2a@(�)@(!@'� @'�*@'�$@'Z�@&��@&�\@&Z�@%�j@%�@%e,@%Y�@%@$��@$]d@#�@#�@#�f@#iD@#;d@#$t@#�@"҉@"�@"d�@"8�@"�@!�)@![W@!�@ �E@ w�@ �@�[@;d@ߤ@��@�+@i�@h
@a|@5?@�^@o @4@�f@��@l"@S�@K^@6@b@�W@�q@l�@J#@H�@�@�c@�X@}V@5?@�@�D@ԕ@�-@�@A @�@�)@��@��@�)@j@1'@��@ƨ@�q@P�@�@��@}V@Ov@_@�@�j@��@��@�X@��@a�@%@��@?�@�@�6@��@\)@33@��@�L@^5@J�@-@��@�-@s�@Vm@:�@@��@l"@:�@@�A@�P@U�@.I@�@ں@��@�!@�r@V@;�@.�@��@��@`B@Dg@=�@#�@��@�e@tT@Xy@A�@/�@~@x@�@  @�+@�w@o�@6z@"�@@
�2@
�r@
�@
��@
��@
�B@
�!@
R�@
)�@	��@	��@	�h@	��@	k�@	-w@֡@oi@*�@@�@��@��@��@~�@X�@U�@n/@dZ@W?@9�@1�@�@��@�+@xl@M�@^5@=q@_@�>@��@@Vm@&�@;@��@��@�I@�@]d@M@A�@4n@M@�@�}@�@�0@��@�[@��@��@��@��@�@s@;d@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B|�B|�B|�B}"B}VB}"B|�B|�B|�B}<B}<B}VB�B�;B��B��B��B��B��B��B��B�B�[B�-B	FYB
�B
U�B
�HB
�=B
�B!�BA�BBBD�BBAB2�B+6B*B)yB4B4�B=qB9�B<�BW�B��B�oB�6BӏB�KB�gB��B��B�B�FBߤB�kBԯB�%B�5B��BqvBZB6�B�B�B
��B
ՁB
��B
��B
�$B
��B
\]B
<jB
�B	�B	�B	��B	��B	��B	�?B	�B	xRB	aB	Q�B	B�B	2-B	 �B	^B��B�$B�$B�zB�^B��B	�B	�B	
�B	�B	�B	hB	
�B	�B	�B	�B	�B	B�PB�wB	B	�B	
�B	B	 B	�B	*�B	/ B	L�B	l�B	x�B	~�B	�B	� B	��B	�pB	�gB	�1B	�BB	��B	�B	�B	�3B	� B	�B	��B	�lB	��B	�8B	�+B	��B	��B	�`B	��B	��B	��B	�B	��B	��B	�*B	�FB	��B	��B	�MB	�2B	�rB	�XB	�rB	��B	�dB	��B	�jB	�jB	�PB	��B	��B	�B	�JB	�xB	��B	�BB	�'B	�%B	ǔB	��B	��B	�B	�qB	�)B	��B	�B	ݘB	ݘB	��B	��B	ބB	�B	�B	��B	��B	�NB	�B	�bB	�NB	�B	ޞB	یB	�1B	�yB	�9B	�+B	چB	��B	�RB	�B	�B	�!B	� B	�UB	�TB	��B	�$B	��B	�RB	�B	��B	��B	�%B	�B	�oB	�B	�B	�B	��B	��B	�MB	�nB	��B	�B	�B	�zB	��B	�fB
?B
B
�B
�B
�B
xB

�B

=B
	�B
	�B

#B

�B

rB

XB

#B
	RB
�B
EB
+B
	B
	7B
	7B
�B
�B
�B
tB
�B
B
�B
uB
�B
 �B
 4B
 �B
UB
�B
B
�B
+B
�B
%B
tB
+B
+B
B
�B
B
�B
YB
�B
�B
B
�B
�B
�B
SB
mB
B
�B
B
�B
3B
�B
B
�B
�B
�B
gB
�B
MB
gB
3B
�B
B
3B
3B
3B
B
3B
�B
MB
B
�B
�B
MB
MB
�B
gB
�B
�B
�B
�B
�B
�B
�B
%B
%B
B
tB
tB
YB
%B
�B
B
zB
zB
�B
�B
�B
�B
fB
	RB

=B

�B

�B

�B

rB

#B
	�B
	�B
	B
	B
	B
B
B

rB

�B

#B

	B

#B

#B

	B

�B
B
�B
�B
�B
B
jB
�B
PB
B
B
B
�B
B
PB
PB
�B
�B
B
.B
�B
�B
B
B
�B
B
}B
4B
 B
NB
B
 B
�B
}B
}B
4B
4B
�B
�B
�B
[B
�B
uB
aB
�B
B
YB
+B
1B
7B
�B
	B
	B
�B
	B
�B
�B
�B
�B
qB
�B
�B
�B
�B
�B
�B
xB
B
)B
B
�B
�B
]B
�B
CB
CB
B
�B
�B
dB
�B
�B
�B
�B
B
OB
�B
�B
�B
B
;B
!B
B
�B
 �B
!HB
!|B
!�B
"4B
"NB
"4B
"�B
"�B
#nB
#�B
#nB
#�B
$@B
$�B
$�B
%B
%B
%zB
%�B
&B
&2B
&LB
&�B
&fB
&fB
&�B
&�B
&�B
'8B
'�B
(XB
(>B
(�B
(�B
)B
)B
)*B
*�B
,B
,=B
,�B
,�B
-B
-B
-)B
-�B
-�B
-�B
-�B
.IB
.cB
.�B
/�B
/iB
/OB
/�B
/iB
/�B
0B
0B
0;B
0�B
1�B
2|B
2�B
3�B
3�B
33B
4TB
4�B
5�B
6FB
6�B
6�B
6�B
6�B
7LB
7�B
8�B
8�B
8�B
9	B
8�B
9rB
9�B
9�B
:B
:xB
;B
;JB
;dB
;B
;�B
;�B
;�B
<PB
<PB
<PB
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>(B
>]B
>]B
>wB
>�B
>�B
>�B
?B
?cB
@4B
@iB
@�B
A�B
A�B
BB
B�B
B�B
C�B
DgB
DMB
D�B
D�B
D�B
D�B
D�B
D�B
EB
D�B
EmB
E�B
E�B
F?B
G+B
GEB
G+B
G+B
G+B
G+B
G+B
G_B
G+B
G�B
HB
H�B
H�B
H�B
H�B
IRB
I�B
I�B
J#B
J=B
J�B
J�B
J�B
J�B
J�B
J�B
JXB
J�B
J�B
J�B
J�B
K�B
K�B
K�B
KxB
K^B
K�B
LJB
LdB
LdB
L0B
L0B
L0B
LdB
L0B
LB
L�B
M�B
M�B
M�B
NB
NVB
N<B
N�B
N<B
NVB
N�B
OBB
OB
N�B
O(B
OB
O�B
O�B
P.B
P.B
P.B
PB
O�B
O�B
O�B
O�B
O�B
PbB
P�B
Q�B
Q�B
Q�B
RTB
R�B
R�B
R�B
R�B
R�B
R�B
S[B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
UMB
U�B
U�B
U�B
UgB
U�B
VB
VmB
VSB
V�B
V�B
V�B
W
B
W?B
XB
XEB
X+B
X_B
X�B
X�B
YB
Y�B
ZB
ZQB
ZkB
ZkB
ZkB
ZkB
Z�B
Z�B
ZQB
ZQB
Z�B
Z�B
Z�B
Z�B
[	B
[=B
[=B
[�B
[�B
\B
\)B
\B
\)B
\CB
\xB
\�B
\�B
\�B
]dB
]�B
]�B
^OB
^�B
^�B
^�B
^�B
^�B
_!B
_�B
_�B
`'B
`\B
`�B
`�B
`�B
aHB
a�B
a�B
a�B
a�B
b�B
b�B
b�B
cTB
cnB
c�B
d&B
d�B
d�B
eB
eFB
eFB
e`B
ezB
e�B
e�B
e�B
e�B
e�B
e�B
f2B
fLB
f�B
f�B
f�B
gB
g8B
g8B
g�B
hXB
h�B
h�B
h�B
h�B
iB
i_B
iyB
i�B
jKB
jKB
j0B
jeB
j�B
j�B
kkB
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l"B
lqB
l�B
l�B
l�B
m]B
m�B
m�B
nB
n/B
n}B
o B
o B
oB
oOB
oiB
oOB
oOB
oiB
o�B
p;B
poB
p�B
p�B
q'B
q[B
qvB
qvB
q�B
q�B
q�B
q�B
q�B
q�B
r-B
r|B
r�B
r�B
r�B
s3B
s3B
s3B
sMB
s�B
s�B
s�B
s�B
tTB
t�B
t�B
utB
u�B
u�B
u�B
u�B
v+B
v`B
v+B
v`B
v`B
v�B
v�B
v�B
wLB
w�B
w�B
w�B
w�B
xB
xlB
x�B
y>B
y>B
y�B
y�B
z*B
zDB
z�B
z�B
z�B
z�B
{0B
{0B
{B
{�B
{�B
{�B
|6B
|�B
|�B
|�B
|�B
}<B
}VB
}�B
}�B
~(B
~BB
~BB
~wB
~�B
~�B
~�B
.B
}B
�B
�B
�B
� B
�OB
�iB
��B
��B
��B
��B
�;B
��B
�oB
��B
��B
��B
��B
��B
��B
�B
�AB
��B
�aB
��B
�B
�B
�SB
��B
��B
��B
��B
��B
��B
��B
��B
�+B
��B
��B
��B
��B
��B
�B
�1B
�KB
�B
�B
�B
�7B
�7B
�lB
�7B
��B
��B
��B
��B
��B
�	B
�=B
�rB
��B
��B
��B
�B
�)B
�DB
��B
��B
��B
��B
�0B
�B
�B
�JB
�dB
��B
��B
��B
�B
�PB
��B
��B
��B
��B
��B
��B
�B
�<B
�"B
�<11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B|�B|�B|�B}"B}VB}"B|�B|�B|�B}<B}<B}VB�B�;B��B��B��B��B��B��B��B�B�[B�-B	FYB
�B
U�B
�HB
�=B
�B!�BA�BBBD�BBAB2�B+6B*B)yB4B4�B=qB9�B<�BW�B��B�oB�6BӏB�KB�gB��B��B�B�FBߤB�kBԯB�%B�5B��BqvBZB6�B�B�B
��B
ՁB
��B
��B
�$B
��B
\]B
<jB
�B	�B	�B	��B	��B	��B	�?B	�B	xRB	aB	Q�B	B�B	2-B	 �B	^B��B�$B�$B�zB�^B��B	�B	�B	
�B	�B	�B	hB	
�B	�B	�B	�B	�B	B�PB�wB	B	�B	
�B	B	 B	�B	*�B	/ B	L�B	l�B	x�B	~�B	�B	� B	��B	�pB	�gB	�1B	�BB	��B	�B	�B	�3B	� B	�B	��B	�lB	��B	�8B	�+B	��B	��B	�`B	��B	��B	��B	�B	��B	��B	�*B	�FB	��B	��B	�MB	�2B	�rB	�XB	�rB	��B	�dB	��B	�jB	�jB	�PB	��B	��B	�B	�JB	�xB	��B	�BB	�'B	�%B	ǔB	��B	��B	�B	�qB	�)B	��B	�B	ݘB	ݘB	��B	��B	ބB	�B	�B	��B	��B	�NB	�B	�bB	�NB	�B	ޞB	یB	�1B	�yB	�9B	�+B	چB	��B	�RB	�B	�B	�!B	� B	�UB	�TB	��B	�$B	��B	�RB	�B	��B	��B	�%B	�B	�oB	�B	�B	�B	��B	��B	�MB	�nB	��B	�B	�B	�zB	��B	�fB
?B
B
�B
�B
�B
xB

�B

=B
	�B
	�B

#B

�B

rB

XB

#B
	RB
�B
EB
+B
	B
	7B
	7B
�B
�B
�B
tB
�B
B
�B
uB
�B
 �B
 4B
 �B
UB
�B
B
�B
+B
�B
%B
tB
+B
+B
B
�B
B
�B
YB
�B
�B
B
�B
�B
�B
SB
mB
B
�B
B
�B
3B
�B
B
�B
�B
�B
gB
�B
MB
gB
3B
�B
B
3B
3B
3B
B
3B
�B
MB
B
�B
�B
MB
MB
�B
gB
�B
�B
�B
�B
�B
�B
�B
%B
%B
B
tB
tB
YB
%B
�B
B
zB
zB
�B
�B
�B
�B
fB
	RB

=B

�B

�B

�B

rB

#B
	�B
	�B
	B
	B
	B
B
B

rB

�B

#B

	B

#B

#B

	B

�B
B
�B
�B
�B
B
jB
�B
PB
B
B
B
�B
B
PB
PB
�B
�B
B
.B
�B
�B
B
B
�B
B
}B
4B
 B
NB
B
 B
�B
}B
}B
4B
4B
�B
�B
�B
[B
�B
uB
aB
�B
B
YB
+B
1B
7B
�B
	B
	B
�B
	B
�B
�B
�B
�B
qB
�B
�B
�B
�B
�B
�B
xB
B
)B
B
�B
�B
]B
�B
CB
CB
B
�B
�B
dB
�B
�B
�B
�B
B
OB
�B
�B
�B
B
;B
!B
B
�B
 �B
!HB
!|B
!�B
"4B
"NB
"4B
"�B
"�B
#nB
#�B
#nB
#�B
$@B
$�B
$�B
%B
%B
%zB
%�B
&B
&2B
&LB
&�B
&fB
&fB
&�B
&�B
&�B
'8B
'�B
(XB
(>B
(�B
(�B
)B
)B
)*B
*�B
,B
,=B
,�B
,�B
-B
-B
-)B
-�B
-�B
-�B
-�B
.IB
.cB
.�B
/�B
/iB
/OB
/�B
/iB
/�B
0B
0B
0;B
0�B
1�B
2|B
2�B
3�B
3�B
33B
4TB
4�B
5�B
6FB
6�B
6�B
6�B
6�B
7LB
7�B
8�B
8�B
8�B
9	B
8�B
9rB
9�B
9�B
:B
:xB
;B
;JB
;dB
;B
;�B
;�B
;�B
<PB
<PB
<PB
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>(B
>]B
>]B
>wB
>�B
>�B
>�B
?B
?cB
@4B
@iB
@�B
A�B
A�B
BB
B�B
B�B
C�B
DgB
DMB
D�B
D�B
D�B
D�B
D�B
D�B
EB
D�B
EmB
E�B
E�B
F?B
G+B
GEB
G+B
G+B
G+B
G+B
G+B
G_B
G+B
G�B
HB
H�B
H�B
H�B
H�B
IRB
I�B
I�B
J#B
J=B
J�B
J�B
J�B
J�B
J�B
J�B
JXB
J�B
J�B
J�B
J�B
K�B
K�B
K�B
KxB
K^B
K�B
LJB
LdB
LdB
L0B
L0B
L0B
LdB
L0B
LB
L�B
M�B
M�B
M�B
NB
NVB
N<B
N�B
N<B
NVB
N�B
OBB
OB
N�B
O(B
OB
O�B
O�B
P.B
P.B
P.B
PB
O�B
O�B
O�B
O�B
O�B
PbB
P�B
Q�B
Q�B
Q�B
RTB
R�B
R�B
R�B
R�B
R�B
R�B
S[B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
UMB
U�B
U�B
U�B
UgB
U�B
VB
VmB
VSB
V�B
V�B
V�B
W
B
W?B
XB
XEB
X+B
X_B
X�B
X�B
YB
Y�B
ZB
ZQB
ZkB
ZkB
ZkB
ZkB
Z�B
Z�B
ZQB
ZQB
Z�B
Z�B
Z�B
Z�B
[	B
[=B
[=B
[�B
[�B
\B
\)B
\B
\)B
\CB
\xB
\�B
\�B
\�B
]dB
]�B
]�B
^OB
^�B
^�B
^�B
^�B
^�B
_!B
_�B
_�B
`'B
`\B
`�B
`�B
`�B
aHB
a�B
a�B
a�B
a�B
b�B
b�B
b�B
cTB
cnB
c�B
d&B
d�B
d�B
eB
eFB
eFB
e`B
ezB
e�B
e�B
e�B
e�B
e�B
e�B
f2B
fLB
f�B
f�B
f�B
gB
g8B
g8B
g�B
hXB
h�B
h�B
h�B
h�B
iB
i_B
iyB
i�B
jKB
jKB
j0B
jeB
j�B
j�B
kkB
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l"B
lqB
l�B
l�B
l�B
m]B
m�B
m�B
nB
n/B
n}B
o B
o B
oB
oOB
oiB
oOB
oOB
oiB
o�B
p;B
poB
p�B
p�B
q'B
q[B
qvB
qvB
q�B
q�B
q�B
q�B
q�B
q�B
r-B
r|B
r�B
r�B
r�B
s3B
s3B
s3B
sMB
s�B
s�B
s�B
s�B
tTB
t�B
t�B
utB
u�B
u�B
u�B
u�B
v+B
v`B
v+B
v`B
v`B
v�B
v�B
v�B
wLB
w�B
w�B
w�B
w�B
xB
xlB
x�B
y>B
y>B
y�B
y�B
z*B
zDB
z�B
z�B
z�B
z�B
{0B
{0B
{B
{�B
{�B
{�B
|6B
|�B
|�B
|�B
|�B
}<B
}VB
}�B
}�B
~(B
~BB
~BB
~wB
~�B
~�B
~�B
.B
}B
�B
�B
�B
� B
�OB
�iB
��B
��B
��B
��B
�;B
��B
�oB
��B
��B
��B
��B
��B
��B
�B
�AB
��B
�aB
��B
�B
�B
�SB
��B
��B
��B
��B
��B
��B
��B
��B
�+B
��B
��B
��B
��B
��B
�B
�1B
�KB
�B
�B
�B
�7B
�7B
�lB
�7B
��B
��B
��B
��B
��B
�	B
�=B
�rB
��B
��B
��B
�B
�)B
�DB
��B
��B
��B
��B
�0B
�B
�B
�JB
�dB
��B
��B
��B
�B
�PB
��B
��B
��B
��B
��B
��B
�B
�<B
�"B
�<11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105250  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192926  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192926  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192926                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042934  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042934  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                