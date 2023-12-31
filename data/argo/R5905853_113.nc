CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:43:02Z creation;2022-06-04T17:43:03Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174302  20220610141505  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               qA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��E�q�1   @��E��/@/'-�cYp��
=1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A���A���A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�33B���B�  B�  B�  B�ffB���B���B�  B�  B���B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C �C�fC  C  C  C	�fC  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*�C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ33C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D��3D�3D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @z�@z�H@�p�@�p�A�RA>�RA^�RA~�RA�\)A�(�A�(�A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'G�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
B�p�B�
=B���B��
B��
B��
B�=pB���B���B��
B��
B���B��
B���B���B��
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
B��
B��
B��
B�
=C C��C�C�C�C	��C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C(C*C+��C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CZ�C[��C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&�{D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qD���D� �D�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD� �D�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��>D�=qD�}qD��qD��qD�:>D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�<�A�<6A�;�A�<6A�<6A�=A�>BA�7�A�49A�5�A�6�A�8�A�)�A� \A��A��A�A�YA��cA��A͉�A�T�A��A��A��A��A��Aɹ�AɧRAɏ�A�~�A�YA�V9A�h
A�)�AǸ�A��A�.IA¤A�2aA���A�xA��A��A�jA���A�q�A���A���A��.A�GA���A��A�9�A��rA�!A�33A�n�A���A��)A�5?A�]dA�%FA��A��(A�`A�wfA�-A���A�r�A�JXA�MA���A�%A���A�՛A�'A��A��<A�	A�LA{J�Ax��AwjAt{JAk��Ah��Af2aAa֡A]l�A\)�A[_AYl�AVf�AS�`AR:*ANE�AL-wAJ�AG�AE�xAD!�ABoA>s�A;�'A:V�A8�-A6jA5_A4y�A3��A3��A3�	A3y�A3iDA3�A21A0�A.�4A,�cA+�<A*]�A)�sA)v�A'e,A%��A%��A%�A%e,A$��A$�"A$�A#�;A"��AqvAkQAOAW�A��A��A��AXyA+�A?�A/A�tA�6A@OA�A�AߤAc�A�A�A�AMAL�Ae,A�A�zA�PA��A��AѷAɆAJ�A��A�AkQA�A�uA�A�<A�AɆA�AخA�FAM�A�A��A��A�A�HA��A<�A$�AMA	��A�^AD�A�A�A��A�A0�ASA�jAI�AL�Ar�A�dAIRAJ�Ab�AC-A�uA�PASA\)A��AA�~AP�A ��A .�@��C@�J#@�@@��@���@��H@��.@��@@��@�8@��`@��b@�d�@�'R@��k@�A @���@��@�x@�,�@��@�l"@�s�@��[@�3�@��@�>�@��o@��@�u@��.@��@��@�@�|�@�@�e,@��@�7@�:�@�ѷ@�{�@�d�@�e�@��a@㯸@�s�@��@��@�=@�,=@ޮ}@�C-@��@�  @� �@�ƨ@ݎ�@�E9@܁o@��o@۠�@�]�@ڬ@��@��6@َ�@�f�@ؾ@���@�w2@� i@�d�@��g@�p�@�+@��@�p;@��r@�+@�YK@�0U@�ƨ@��@��@��A@��z@�&�@Ϋ6@�>B@ͿH@�a�@̵�@�O@˫�@���@ɍP@�tT@�@O@Ƣ4@��@ŨX@�u�@��p@�.�@���@�x�@½<@�.�@�j�@��|@���@�  @���@�^�@���@��@��@���@��@�J�@���@���@�%F@��'@�D�@���@��P@�x�@�E9@���@��4@�z@��@�qv@�4@��@��r@���@���@�Q�@��@���@�"h@��h@�5�@���@���@��*@�/�@���@���@�0U@��@�5�@���@�6@�C�@�C@��@�l"@�U2@�Ft@��@���@�W?@��,@���@��@�e,@���@�^5@�@���@�N<@���@���@�K^@���@�o�@�P�@�7L@���@��@��{@�%F@��j@�z@�2�@��6@��M@�RT@��@��F@�D�@��@���@��[@�a@��@���@�tT@�6@��@��'@��~@��7@�X�@�	l@���@�YK@��@��)@��z@��4@�b�@�B�@�6z@�RT@��@��]@��R@�H@���@�8�@� �@�  @��@���@���@�F@�@��M@���@��b@�	�@�1�@�@@��@��@��@�YK@�"h@�!@�ϫ@�J#@���@���@�'R@��;@�˒@���@�/@��E@��@�Ta@�@�]�@��@��@��@�ѷ@���@��1@�q@�9X@��+@���@��@@���@�1�@���@���@�`�@�+k@���@��@��@�B�@���@��Y@��@�ƨ@�qv@�/�@�S@��s@���@�m�@�B[@�4@���@�k�@�0�@��@��v@�� @�`�@�@��z@��@@�w2@�8�@�@���@�ں@��@�~(@�H�@�&�@���@��S@�H�@�'�@��@��\@�l"@�K^@��@��F@��$@�{J@�:�@��@��"@��]@���@�c @�/�@��
@��:@�x�@�Vm@��@���@��}@���@�kQ@�%�@��@��@�a@�*0@���@��@�� @�6�@��@~5?@}�@}��@}�-@}��@}��@}B�@|tT@{��@{�@z��@z=q@y�X@ye,@y+@x��@xbN@w�K@wX�@vں@v�\@vTa@u�#@u�@u;@t�@t6@t-�@s�K@s��@sS@r��@r_�@q��@p�)@pS�@o��@og�@o;d@nn�@mj@l��@l]d@l@k��@kg�@k�@j��@j�@i�X@ik�@h�@h�I@hN�@h�@g�W@g�}@g��@gb�@g�@f�X@fs�@e��@e%F@d��@dV�@d�@c�@c��@ciD@c+@b\�@b4@a��@a*0@`�@_��@_�@_�@_�@_e�@^�"@^&�@]�@]�"@]�@\Ĝ@\l"@[��@[خ@[{J@[)_@[�@Z�8@Z�8@Z��@Zd�@ZGE@Z�@Y�@Y��@Yzx@Y:�@Y�@X�@X�j@Xz�@X<�@W�]@W˒@W�q@W�:@WA�@W�@V�@VQ@V#:@U�#@U}�@T��@Tc�@T�@S�}@S��@R�8@RH�@Q��@QT�@Pѷ@P�@P��@PQ�@P(�@O��@O4�@N�@N�@N@M�t@Mc@Mj@MVm@MV@L�5@Lی@L�@Lw�@Kخ@KMj@J�m@J�@Iu�@I�@H�@HC-@G�6@G�k@Gv`@G"�@F��@F��@FL0@F�@E��@E��@E!�@D��@Cݘ@C~�@CX�@C�@B��@B�'@B�<@B�@B�1@BTa@Aԕ@A��@A��@A:�@A�@@�f@@��@@�p@@�.@@H@?��@?b�@?33@>�@>�m@>�+@>L0@=��@<�	@<Z@<1'@;�A@;iD@;�@:��@:3�@9�.@9�d@9�M@9?}@8Ɇ@8�@8c�@8%�@7�@7��@7X�@7&@6��@6C�@64@5�.@5�9@5�@5�=@5e,@5�@4��@4Ft@4"h@3�;@3�[@3a@2ߤ@2h
@2@1�7@1�@0�5@0��@0�@0]d@0(�@/��@/��@.�M@.+k@-��@-B�@-(�@-q@,��@,��@,M@+��@+y�@+'�@*�c@*ߤ@*�@*��@*Z�@)��@)��@)a�@)�@(�9@(j@(b@'��@'!-@' i@&҉@&YK@%��@%��@%f�@%/@$�K@$Ɇ@$c�@$PH@$2�@#��@#��@#W?@#8@#,�@#�@"��@"��@"Ov@!��@!ԕ@!�h@!p�@!X@!�@ �O@ A�@��@�$@y�@X�@��@�@u%@5?@�@ϫ@��@��@�M@w2@e,@G�@�@��@�@��@�@�W@ݘ@��@��@��@K�@҉@u%@C�@@��@e,@A @@�O@6@  @�@�}@��@��@o�@4�@�@�'@Ta@�Z@��@f�@?}@4@@@�p@Ɇ@|�@M@4n@-�@�@��@Z�@=@4�@�@�@��@v�@\�@=q@�@�@�@|@f�@[W@[W@ \@�@�@�u@z�@6@�A@� @��@�F@��@v`@'�@�@�'@��@�b@Z�@)�@#:@	@�@��@�9@@|@B�@0�@%@�@�)@~(@:�@%�@7@	�@�@��@�A@�;@�6@�@��@��@�@
�,@
�!@
��@
E�@	��@	��@	@	��@	��@	o @	O�@	B�@	-w@	q@�/@�4@�o@e�@e�@_@4n@1@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�<�A�<6A�;�A�<6A�<6A�=A�>BA�7�A�49A�5�A�6�A�8�A�)�A� \A��A��A�A�YA��cA��A͉�A�T�A��A��A��A��A��Aɹ�AɧRAɏ�A�~�A�YA�V9A�h
A�)�AǸ�A��A�.IA¤A�2aA���A�xA��A��A�jA���A�q�A���A���A��.A�GA���A��A�9�A��rA�!A�33A�n�A���A��)A�5?A�]dA�%FA��A��(A�`A�wfA�-A���A�r�A�JXA�MA���A�%A���A�՛A�'A��A��<A�	A�LA{J�Ax��AwjAt{JAk��Ah��Af2aAa֡A]l�A\)�A[_AYl�AVf�AS�`AR:*ANE�AL-wAJ�AG�AE�xAD!�ABoA>s�A;�'A:V�A8�-A6jA5_A4y�A3��A3��A3�	A3y�A3iDA3�A21A0�A.�4A,�cA+�<A*]�A)�sA)v�A'e,A%��A%��A%�A%e,A$��A$�"A$�A#�;A"��AqvAkQAOAW�A��A��A��AXyA+�A?�A/A�tA�6A@OA�A�AߤAc�A�A�A�AMAL�Ae,A�A�zA�PA��A��AѷAɆAJ�A��A�AkQA�A�uA�A�<A�AɆA�AخA�FAM�A�A��A��A�A�HA��A<�A$�AMA	��A�^AD�A�A�A��A�A0�ASA�jAI�AL�Ar�A�dAIRAJ�Ab�AC-A�uA�PASA\)A��AA�~AP�A ��A .�@��C@�J#@�@@��@���@��H@��.@��@@��@�8@��`@��b@�d�@�'R@��k@�A @���@��@�x@�,�@��@�l"@�s�@��[@�3�@��@�>�@��o@��@�u@��.@��@��@�@�|�@�@�e,@��@�7@�:�@�ѷ@�{�@�d�@�e�@��a@㯸@�s�@��@��@�=@�,=@ޮ}@�C-@��@�  @� �@�ƨ@ݎ�@�E9@܁o@��o@۠�@�]�@ڬ@��@��6@َ�@�f�@ؾ@���@�w2@� i@�d�@��g@�p�@�+@��@�p;@��r@�+@�YK@�0U@�ƨ@��@��@��A@��z@�&�@Ϋ6@�>B@ͿH@�a�@̵�@�O@˫�@���@ɍP@�tT@�@O@Ƣ4@��@ŨX@�u�@��p@�.�@���@�x�@½<@�.�@�j�@��|@���@�  @���@�^�@���@��@��@���@��@�J�@���@���@�%F@��'@�D�@���@��P@�x�@�E9@���@��4@�z@��@�qv@�4@��@��r@���@���@�Q�@��@���@�"h@��h@�5�@���@���@��*@�/�@���@���@�0U@��@�5�@���@�6@�C�@�C@��@�l"@�U2@�Ft@��@���@�W?@��,@���@��@�e,@���@�^5@�@���@�N<@���@���@�K^@���@�o�@�P�@�7L@���@��@��{@�%F@��j@�z@�2�@��6@��M@�RT@��@��F@�D�@��@���@��[@�a@��@���@�tT@�6@��@��'@��~@��7@�X�@�	l@���@�YK@��@��)@��z@��4@�b�@�B�@�6z@�RT@��@��]@��R@�H@���@�8�@� �@�  @��@���@���@�F@�@��M@���@��b@�	�@�1�@�@@��@��@��@�YK@�"h@�!@�ϫ@�J#@���@���@�'R@��;@�˒@���@�/@��E@��@�Ta@�@�]�@��@��@��@�ѷ@���@��1@�q@�9X@��+@���@��@@���@�1�@���@���@�`�@�+k@���@��@��@�B�@���@��Y@��@�ƨ@�qv@�/�@�S@��s@���@�m�@�B[@�4@���@�k�@�0�@��@��v@�� @�`�@�@��z@��@@�w2@�8�@�@���@�ں@��@�~(@�H�@�&�@���@��S@�H�@�'�@��@��\@�l"@�K^@��@��F@��$@�{J@�:�@��@��"@��]@���@�c @�/�@��
@��:@�x�@�Vm@��@���@��}@���@�kQ@�%�@��@��@�a@�*0@���@��@�� @�6�@��@~5?@}�@}��@}�-@}��@}��@}B�@|tT@{��@{�@z��@z=q@y�X@ye,@y+@x��@xbN@w�K@wX�@vں@v�\@vTa@u�#@u�@u;@t�@t6@t-�@s�K@s��@sS@r��@r_�@q��@p�)@pS�@o��@og�@o;d@nn�@mj@l��@l]d@l@k��@kg�@k�@j��@j�@i�X@ik�@h�@h�I@hN�@h�@g�W@g�}@g��@gb�@g�@f�X@fs�@e��@e%F@d��@dV�@d�@c�@c��@ciD@c+@b\�@b4@a��@a*0@`�@_��@_�@_�@_�@_e�@^�"@^&�@]�@]�"@]�@\Ĝ@\l"@[��@[خ@[{J@[)_@[�@Z�8@Z�8@Z��@Zd�@ZGE@Z�@Y�@Y��@Yzx@Y:�@Y�@X�@X�j@Xz�@X<�@W�]@W˒@W�q@W�:@WA�@W�@V�@VQ@V#:@U�#@U}�@T��@Tc�@T�@S�}@S��@R�8@RH�@Q��@QT�@Pѷ@P�@P��@PQ�@P(�@O��@O4�@N�@N�@N@M�t@Mc@Mj@MVm@MV@L�5@Lی@L�@Lw�@Kخ@KMj@J�m@J�@Iu�@I�@H�@HC-@G�6@G�k@Gv`@G"�@F��@F��@FL0@F�@E��@E��@E!�@D��@Cݘ@C~�@CX�@C�@B��@B�'@B�<@B�@B�1@BTa@Aԕ@A��@A��@A:�@A�@@�f@@��@@�p@@�.@@H@?��@?b�@?33@>�@>�m@>�+@>L0@=��@<�	@<Z@<1'@;�A@;iD@;�@:��@:3�@9�.@9�d@9�M@9?}@8Ɇ@8�@8c�@8%�@7�@7��@7X�@7&@6��@6C�@64@5�.@5�9@5�@5�=@5e,@5�@4��@4Ft@4"h@3�;@3�[@3a@2ߤ@2h
@2@1�7@1�@0�5@0��@0�@0]d@0(�@/��@/��@.�M@.+k@-��@-B�@-(�@-q@,��@,��@,M@+��@+y�@+'�@*�c@*ߤ@*�@*��@*Z�@)��@)��@)a�@)�@(�9@(j@(b@'��@'!-@' i@&҉@&YK@%��@%��@%f�@%/@$�K@$Ɇ@$c�@$PH@$2�@#��@#��@#W?@#8@#,�@#�@"��@"��@"Ov@!��@!ԕ@!�h@!p�@!X@!�@ �O@ A�@��@�$@y�@X�@��@�@u%@5?@�@ϫ@��@��@�M@w2@e,@G�@�@��@�@��@�@�W@ݘ@��@��@��@K�@҉@u%@C�@@��@e,@A @@�O@6@  @�@�}@��@��@o�@4�@�@�'@Ta@�Z@��@f�@?}@4@@@�p@Ɇ@|�@M@4n@-�@�@��@Z�@=@4�@�@�@��@v�@\�@=q@�@�@�@|@f�@[W@[W@ \@�@�@�u@z�@6@�A@� @��@�F@��@v`@'�@�@�'@��@�b@Z�@)�@#:@	@�@��@�9@@|@B�@0�@%@�@�)@~(@:�@%�@7@	�@�@��@�A@�;@�6@�@��@��@�@
�,@
�!@
��@
E�@	��@	��@	@	��@	��@	o @	O�@	B�@	-w@	q@�/@�4@�o@e�@e�@_@4n@1@�@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B�yB��B�B�EB�+B��B�sB�YB�
B��B��B��B��B�1B�BB�B��B	�B		B	�B	PB	<B	<B	pB	~B	�B	3�B	<�B	�~B
�B
I�B
[WB
�oB
�B
��B8�Bv`B}�B� B�.B��B�VB�B�B��B��B�sB��B��B�WB��B�'B�^B��B��B��B��B�YBlWBF�B_B
�B
�4B
�=B
��B
|jB
aB
O\B
BB
5�B
'�B
)B	��B	��B	�B	ƨB	�qB	�_B	{JB	ezB	TFB	<�B	)�B	�B	�B	
=B�lB�B�,B��B�CB�OB�0B�B�B�BՁBЗB�.B�:BڠB��B�B��B��B�B��B�IB�IB�GB��B		RB	{B	"�B	0�B	2B	8B	Z�B	s3B	tB	u�B	w�B	yXB	��B	�xB	�B	��B	��B	�0B	��B	�B	��B	ΥB	ٴB	�nB	�B	�B	ںB	��B	�5B	��B	�B	ޞB	�\B	��B	��B	��B	��B	��B	��B	�B	�vB	�B	��B	��B	��B	�B	��B	�B	�AB	�B	��B	�FB	�?B	�-B	�B	�B	�xB	��B	�HB
gB
YB

	B

=B
	�B
�B
{B
AB
�B
�B
B	�`B	�B	�wB	�oB	�sB	�B	�B	�B	�B	�B	�B	�0B	��B	�B	�"B	�"B	�B
�B
MB
�B	�VB	�	B	��B	�3B	�nB	�rB	��B	��B	�-B	�B	��B	��B	��B	�HB	�}B	��B	��B	�dB	�JB	�JB	�JB	�0B	��B	�*B	��B	��B	��B	��B	�fB	�lB	�8B	��B	��B	��B	�fB	�2B	�3B	�AB	�!B	�iB	�;B	�B	�TB	�fB	�B	�TB	��B	�nB	�B	�B	�B	�B	�CB	�[B	�B	�B	�[B	�B	�B	��B	�vB	�'B	�B	��B	�B	��B	��B	�`B	�fB	�zB	��B	�?B	�%B	�TB	�TB	�hB	�-B	�oB	��B	�B	��B	��B	��B	�+B	��B	��B	��B	��B	�fB	��B	��B	��B	�zB	�`B	��B	��B	�+B	��B	��B	�?B	�`B	�fB	��B	�ZB	�MB	�AB	��B	�WB	�B	�B	�"B	��B	�yB	�B	�eB	�eB	��B	�B	�KB	�QB	��B	�CB	��B	��B	�B	��B	�B	�!B	�B	�B	�>B	��B	�DB	��B	��B	�B	�B	�0B	��B	�B	�dB	�jB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�qB	�wB	��B	�(B	�B	��B	��B	�]B	��B	�wB	��B
 iB	��B	��B
 B
 �B
 �B
 B
 �B
 �B	��B
 OB
 �B
;B
oB
�B
�B
�B
�B
�B
�B
-B
�B
�B
{B
�B
�B
gB
�B
�B
�B
�B
B
�B
mB
SB
�B
?B
�B
B
%B
�B
�B
�B
EB
�B
KB
1B
B
fB
�B
�B
KB
�B
�B
�B
	RB
	lB
	�B

�B
�B
JB
6B
�B
B
JB
B
@B
[B
�B
�B
�B
:B
FB
aB
FB
�B
[B
�B
oB
�B
�B
�B
�B
�B
eB
�B
�B
sB
�B
SB
$B
B
�B
�B
)B
xB
�B
�B
dB
B
dB
�B
�B
�B
OB
�B
�B
�B
�B
�B
;B
�B
�B
 'B
 \B
 �B
 �B
 �B
!-B
!B
!�B
!�B
"hB
"�B
#TB
#�B
#�B
#�B
#�B
$@B
$�B
$�B
%�B
%�B
&�B
&�B
'8B
'�B
'�B
(XB
(�B
(�B
)*B
(�B
)*B
)yB
)yB
)�B
*0B
*�B
*�B
*�B
+kB
+�B
+�B
,=B
,�B
,�B
,�B
-B
-wB
-]B
-�B
-�B
.IB
.B
.cB
.cB
.�B
.�B
/5B
/�B
/iB
/�B
/�B
0B
0;B
0UB
0oB
0�B
0�B
1B
1[B
1�B
1�B
2B
2-B
2�B
3�B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5�B
5�B
6+B
6FB
6`B
6�B
6�B
7B
7fB
7�B
8B
8lB
8�B
8�B
9	B
9�B
9�B
9�B
9�B
:B
:�B
;JB
;JB
;�B
;�B
;�B
<B
;�B
<B
<B
<6B
<6B
<�B
=VB
=qB
=�B
>(B
>(B
>�B
>�B
?.B
?�B
?�B
?�B
@B
@4B
@OB
@�B
AB
AB
AUB
A�B
A�B
AUB
AoB
BuB
C-B
CGB
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E9B
F�B
F�B
F?B
F?B
F?B
FYB
F%B
GB
HKB
HKB
HfB
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J	B
J#B
J#B
J=B
JXB
JXB
J�B
J�B
J�B
J�B
J�B
KxB
KxB
K�B
K�B
K�B
LB
LdB
LJB
L~B
L�B
L�B
L�B
MB
MB
M6B
M�B
M�B
M�B
NpB
NVB
NpB
OB
O\B
OvB
OvB
O�B
O�B
P�B
P�B
Q4B
QNB
Q�B
R B
RB
R B
R B
RTB
RoB
RoB
RoB
R�B
S&B
S[B
S�B
TB
T{B
T�B
U2B
UMB
U�B
U�B
VB
VSB
V�B
V�B
W
B
W$B
WsB
WYB
W�B
XB
X�B
X�B
X�B
Y1B
YeB
YeB
YeB
YKB
YeB
Y�B
Y�B
ZB
Z7B
ZQB
ZkB
ZkB
ZkB
Z�B
Z�B
Z�B
[=B
[�B
[�B
[�B
[�B
[�B
[�B
\]B
]/B
]~B
]�B
]�B
^OB
^OB
^�B
_!B
_!B
_;B
_�B
_�B
`B
_�B
`'B
`\B
`\B
`�B
`�B
`�B
`�B
`�B
`�B
aB
a-B
a-B
aHB
aHB
a�B
a�B
b4B
bNB
bhB
bhB
b�B
cB
cTB
cnB
c�B
dtB
dZB
d�B
d�B
d�B
eB
ezB
e`B
e�B
f2B
f�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
g�B
h>B
h>B
h>B
h>B
hXB
h�B
iB
iB
iyB
i_B
iyB
iyB
i�B
i�B
j�B
jB
j�B
kB
k�B
lB
k�B
l"B
lB
l=B
l�B
l�B
l�B
l�B
m)B
mCB
m]B
mwB
m]B
mwB
m�B
nB
ncB
n}B
n�B
n�B
n�B
o B
oOB
o�B
p;B
pUB
pUB
p�B
qB
q[B
qAB
qvB
q�B
q�B
rB
rB
rB
rB
rB
r-B
raB
raB
rGB
r�B
sB
s3B
s3B
sMB
shB
sMB
s�B
tB
tTB
tTB
t�B
uB
uZB
uZB
u�B
u�B
vzB
vzB
v�B
v�B
v�B
v�B
v�B
wLB
w�B
xB
xlB
xB
xRB
x�B
x�B
x�B
y$B
yrB
y�B
zB
z*B
z*B
z*B
z*B
z*B
z�B
{B
{JB
{B
{�B
{�B
|B
|B
|B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}<B
}VB
}VB
}�B
}�B
}�B
}�B
}�B
~B
~(B
~�B
B
cB
}B
�B
�B
�OB
�OB
�iB
�iB
��B
��B
��B
�B
�;B
�;B
�oB
��B
��B
��B
�AB
�AB
�AB
�uB
�uB
�uB
�[B
��B
�uB
��B
��B
��B
��B
��B
�-B
�aB
��B
��B
��B
��B
��B
��B
�3B
�MB
�MB
�gB
��B
��B
�B
�B
�B
��B
��B
�%B
�%B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B�yB��B�B�EB�+B��B�sB�YB�
B��B��B��B��B�1B�BB�B��B	�B		B	�B	PB	<B	<B	pB	~B	�B	3�B	<�B	�~B
�B
I�B
[WB
�oB
�B
��B8�Bv`B}�B� B�.B��B�VB�B�B��B��B�sB��B��B�WB��B�'B�^B��B��B��B��B�YBlWBF�B_B
�B
�4B
�=B
��B
|jB
aB
O\B
BB
5�B
'�B
)B	��B	��B	�B	ƨB	�qB	�_B	{JB	ezB	TFB	<�B	)�B	�B	�B	
=B�lB�B�,B��B�CB�OB�0B�B�B�BՁBЗB�.B�:BڠB��B�B��B��B�B��B�IB�IB�GB��B		RB	{B	"�B	0�B	2B	8B	Z�B	s3B	tB	u�B	w�B	yXB	��B	�xB	�B	��B	��B	�0B	��B	�B	��B	ΥB	ٴB	�nB	�B	�B	ںB	��B	�5B	��B	�B	ޞB	�\B	��B	��B	��B	��B	��B	��B	�B	�vB	�B	��B	��B	��B	�B	��B	�B	�AB	�B	��B	�FB	�?B	�-B	�B	�B	�xB	��B	�HB
gB
YB

	B

=B
	�B
�B
{B
AB
�B
�B
B	�`B	�B	�wB	�oB	�sB	�B	�B	�B	�B	�B	�B	�0B	��B	�B	�"B	�"B	�B
�B
MB
�B	�VB	�	B	��B	�3B	�nB	�rB	��B	��B	�-B	�B	��B	��B	��B	�HB	�}B	��B	��B	�dB	�JB	�JB	�JB	�0B	��B	�*B	��B	��B	��B	��B	�fB	�lB	�8B	��B	��B	��B	�fB	�2B	�3B	�AB	�!B	�iB	�;B	�B	�TB	�fB	�B	�TB	��B	�nB	�B	�B	�B	�B	�CB	�[B	�B	�B	�[B	�B	�B	��B	�vB	�'B	�B	��B	�B	��B	��B	�`B	�fB	�zB	��B	�?B	�%B	�TB	�TB	�hB	�-B	�oB	��B	�B	��B	��B	��B	�+B	��B	��B	��B	��B	�fB	��B	��B	��B	�zB	�`B	��B	��B	�+B	��B	��B	�?B	�`B	�fB	��B	�ZB	�MB	�AB	��B	�WB	�B	�B	�"B	��B	�yB	�B	�eB	�eB	��B	�B	�KB	�QB	��B	�CB	��B	��B	�B	��B	�B	�!B	�B	�B	�>B	��B	�DB	��B	��B	�B	�B	�0B	��B	�B	�dB	�jB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�qB	�wB	��B	�(B	�B	��B	��B	�]B	��B	�wB	��B
 iB	��B	��B
 B
 �B
 �B
 B
 �B
 �B	��B
 OB
 �B
;B
oB
�B
�B
�B
�B
�B
�B
-B
�B
�B
{B
�B
�B
gB
�B
�B
�B
�B
B
�B
mB
SB
�B
?B
�B
B
%B
�B
�B
�B
EB
�B
KB
1B
B
fB
�B
�B
KB
�B
�B
�B
	RB
	lB
	�B

�B
�B
JB
6B
�B
B
JB
B
@B
[B
�B
�B
�B
:B
FB
aB
FB
�B
[B
�B
oB
�B
�B
�B
�B
�B
eB
�B
�B
sB
�B
SB
$B
B
�B
�B
)B
xB
�B
�B
dB
B
dB
�B
�B
�B
OB
�B
�B
�B
�B
�B
;B
�B
�B
 'B
 \B
 �B
 �B
 �B
!-B
!B
!�B
!�B
"hB
"�B
#TB
#�B
#�B
#�B
#�B
$@B
$�B
$�B
%�B
%�B
&�B
&�B
'8B
'�B
'�B
(XB
(�B
(�B
)*B
(�B
)*B
)yB
)yB
)�B
*0B
*�B
*�B
*�B
+kB
+�B
+�B
,=B
,�B
,�B
,�B
-B
-wB
-]B
-�B
-�B
.IB
.B
.cB
.cB
.�B
.�B
/5B
/�B
/iB
/�B
/�B
0B
0;B
0UB
0oB
0�B
0�B
1B
1[B
1�B
1�B
2B
2-B
2�B
3�B
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5�B
5�B
6+B
6FB
6`B
6�B
6�B
7B
7fB
7�B
8B
8lB
8�B
8�B
9	B
9�B
9�B
9�B
9�B
:B
:�B
;JB
;JB
;�B
;�B
;�B
<B
;�B
<B
<B
<6B
<6B
<�B
=VB
=qB
=�B
>(B
>(B
>�B
>�B
?.B
?�B
?�B
?�B
@B
@4B
@OB
@�B
AB
AB
AUB
A�B
A�B
AUB
AoB
BuB
C-B
CGB
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E9B
F�B
F�B
F?B
F?B
F?B
FYB
F%B
GB
HKB
HKB
HfB
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
J	B
J#B
J#B
J=B
JXB
JXB
J�B
J�B
J�B
J�B
J�B
KxB
KxB
K�B
K�B
K�B
LB
LdB
LJB
L~B
L�B
L�B
L�B
MB
MB
M6B
M�B
M�B
M�B
NpB
NVB
NpB
OB
O\B
OvB
OvB
O�B
O�B
P�B
P�B
Q4B
QNB
Q�B
R B
RB
R B
R B
RTB
RoB
RoB
RoB
R�B
S&B
S[B
S�B
TB
T{B
T�B
U2B
UMB
U�B
U�B
VB
VSB
V�B
V�B
W
B
W$B
WsB
WYB
W�B
XB
X�B
X�B
X�B
Y1B
YeB
YeB
YeB
YKB
YeB
Y�B
Y�B
ZB
Z7B
ZQB
ZkB
ZkB
ZkB
Z�B
Z�B
Z�B
[=B
[�B
[�B
[�B
[�B
[�B
[�B
\]B
]/B
]~B
]�B
]�B
^OB
^OB
^�B
_!B
_!B
_;B
_�B
_�B
`B
_�B
`'B
`\B
`\B
`�B
`�B
`�B
`�B
`�B
`�B
aB
a-B
a-B
aHB
aHB
a�B
a�B
b4B
bNB
bhB
bhB
b�B
cB
cTB
cnB
c�B
dtB
dZB
d�B
d�B
d�B
eB
ezB
e`B
e�B
f2B
f�B
f�B
f�B
f�B
gB
g�B
g�B
g�B
g�B
h>B
h>B
h>B
h>B
hXB
h�B
iB
iB
iyB
i_B
iyB
iyB
i�B
i�B
j�B
jB
j�B
kB
k�B
lB
k�B
l"B
lB
l=B
l�B
l�B
l�B
l�B
m)B
mCB
m]B
mwB
m]B
mwB
m�B
nB
ncB
n}B
n�B
n�B
n�B
o B
oOB
o�B
p;B
pUB
pUB
p�B
qB
q[B
qAB
qvB
q�B
q�B
rB
rB
rB
rB
rB
r-B
raB
raB
rGB
r�B
sB
s3B
s3B
sMB
shB
sMB
s�B
tB
tTB
tTB
t�B
uB
uZB
uZB
u�B
u�B
vzB
vzB
v�B
v�B
v�B
v�B
v�B
wLB
w�B
xB
xlB
xB
xRB
x�B
x�B
x�B
y$B
yrB
y�B
zB
z*B
z*B
z*B
z*B
z*B
z�B
{B
{JB
{B
{�B
{�B
|B
|B
|B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}<B
}VB
}VB
}�B
}�B
}�B
}�B
}�B
~B
~(B
~�B
B
cB
}B
�B
�B
�OB
�OB
�iB
�iB
��B
��B
��B
�B
�;B
�;B
�oB
��B
��B
��B
�AB
�AB
�AB
�uB
�uB
�uB
�[B
��B
�uB
��B
��B
��B
��B
��B
�-B
�aB
��B
��B
��B
��B
��B
��B
�3B
�MB
�MB
�gB
��B
��B
�B
�B
�B
��B
��B
�%B
�%B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104930  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174302  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174303  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174303                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024310  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024310  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                