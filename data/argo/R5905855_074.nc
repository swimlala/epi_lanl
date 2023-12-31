CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:23:41Z creation;2022-06-04T19:23:42Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192341  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               JA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�_r0*z1   @�_r���@-1&�x��c�dZ�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A���A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�ffB���B���B�  B�  B�33B�33B�33B�33B�  B�  B���B�33B���B���B�  B�  B�  B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C 33C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.33C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@z�@z�H@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�(�A�\)A�\)A�(�A��\B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
B�=pB�p�B���B��
B��
B�
=B�
=B�
=B�
=B��
B��
B�p�B�
=B���B���B��
B��
B��
B�=pBӣ�Bף�B��
B��
B��
B��
B��
B��
B��
B��
B��
C �C��C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C.�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}��C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D$GD$�GD$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D� �D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�@�D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aʻ0Aʾ�AʾwA���A��[A���A���A�ŢA��A��?A��mA���A�ƨA��A�ɆA��A��)A��dA���A���A��}A�ΥA��HA���A���A���A��[A��aA���A��A��,A��gAʥ�A�/A�>AÐbA�K�A�qAA�A���A���A��:A�bA���A��6A�e�A��A�@A���A�qA���A�iyA�GA�#:A�A�A��jA���A���A��A�:�A���Ay��Aq�jAnU2Aml�Ak��AiQ�AdخAc�Ac�KAaJ�A]�KAY�UAWv�AV��AVn�AU��AT)�AO�UALFAHE9AHbAGw2AEYKA?��A<8A;A�A9j�A:�A:�A9��A8@�A8��A8�+A8L�A8�A6;dA5��A5}�A3�hA2OvA1iDA0��A/ �A-oiA+�,A*&A)��A)~A(�)A(��A*�tA*��A*��A*�TA*~�A*��A*O�A)��A)�A)!�A'�sA%�A%��A%�A$�A$��A$
�A#��A#��A#?A!\�A u�A bA�$AXyA�Ae�A\�Av�A�A�oA��A+A�5A��AJ#A
=A��AZ�A[WA�:A2aAuA�A��AXyA%A��Ab�A+kA�+A��A@�A�=A2aA�"AیAcA֡A~AN<A�A��AYKA��AtTA�A��AiDAOA
�EA
p�A
,=A	��A	~AT�AE�Ax�A��Al�A:�A��A��A��A�A��A�SA�2AzAXyA`�A�A�4A"�A��A�A�\AC-A�A��A�HA��A��A
=A _�@��"@�ی@�ff@�e�@���@�T�@��@��@���@�ԕ@��F@�=@�8@�g8@�4@�o@��9@�/�@�@��/@�A�@���@�Ɇ@��3@�Y�@���@�@�@뽥@�"�@��s@�$@�(�@�A @�Q�@�1@�r@�O@��#@��@�`B@�F@�>B@��Q@�t�@�!�@�z@�=@�@@��@�-�@��@�'�@��D@ߩ�@�K�@�kQ@�rG@��B@�($@��@۶F@ۙ�@�K�@ڮ}@�~(@�V@��@ٿH@ٍP@�X@ص�@�u@ת�@�g�@֮}@չ�@�A�@��@��@Թ�@�.�@��@Ӊ7@�Vm@�*0@��@҇�@�$�@Ѳ-@я�@�s@��@Гu@�=q@�u@�k�@��`@�d�@�P�@�r�@���@��@���@���@�`B@���@�e�@���@��@��Q@�J#@��@Ƭ@�*�@şV@��@�[�@��+@�� @�t�@�;@«6@�g8@���@�l�@��H@�Z@�6@���@�|@�S�@�oi@��@�1@���@��@��@��p@�|�@��@�A�@��F@�e�@�I�@��@���@���@�#�@�ѷ@��@���@�=q@���@���@��@���@�g8@�8�@�{@�@�9�@��,@�l�@�@��@��Z@���@��-@���@�e,@��X@��Y@�&�@��+@�ݘ@���@��-@���@���@�e,@��E@��b@�u�@��@���@�J#@���@��@���@�A�@���@��X@�e�@��@�5?@�خ@�p�@��@��y@���@���@��b@�^5@��@�H�@���@�#:@��@�˒@��@��\@�K^@� �@��X@���@�H�@��Z@�x�@��H@���@�u%@�5?@�]�@��@���@��@��@@�s�@��@��@�I�@�_@��
@���@�w2@���@�I�@���@���@�iD@�W?@�A @�+@���@��U@�tT@�?�@��V@��X@���@���@��D@��+@��@�q@�\�@�Ft@�@��t@�>�@��@��f@���@��I@��@�]d@�&�@�@��&@��S@�;@��B@��@�p;@�!@�ԕ@���@���@�W?@��@��@���@���@�[�@�!�@���@���@���@��	@��@���@�M@��&@��"@�_p@�4@��@��L@�4n@��q@��@�/@���@�@��T@��V@���@�g�@�\)@�U�@�o@�ȴ@�{�@�Z@�b@���@���@�q�@�c @�GE@�4@��Z@�˒@��=@�a�@�0�@��B@��D@�A�@�  @��@�X@� i@��v@��L@�.�@�@��k@�(@�oi@��@���@�7L@���@�s�@��@��@\)@C@~�F@~B[@}��@|��@{��@{{J@z�@za|@z)�@yx�@y+@xK^@wU�@v��@v^5@v�@u��@uo @u�@tm�@s�@s@O@s�@r�<@ra|@q�@p�@p9X@o�@o��@oRT@o"�@n�@n��@nOv@n@m��@mhs@l_@k�	@kH�@j��@j��@j��@j�@j{�@jM�@j@i�@iw2@h��@hS�@h1@hG@g�Q@g{J@g@O@f��@f�@f��@f{�@f�@e�j@d�f@d[�@dN�@c��@cdZ@b�"@b��@b�@bn�@b6�@a��@`Ĝ@`��@`Q�@`2�@` �@`�@_��@_$t@^�H@^@�@]�h@]8�@]�@\�/@\N�@[�r@[+@Z��@ZO@Y�@Y:�@X��@X9X@W�@W�q@WH�@V��@V�@Uc�@U�@T�9@Th�@S�@So�@R�@Rq�@R@�@R.�@R@Q�@Q=�@QV@P�|@P�U@PH@O_p@Oo@N�@Nd�@N6�@M�o@M8�@MV@L�@L�@LK^@K�
@K_p@K!-@J�@J�F@J@I�@I/@Hی@H�@H��@HQ�@H6@H7@Ga@F�\@FL0@F@E�n@EY�@E�@D��@D�Y@Dh�@D~@Cy�@B��@Bl�@B
�@AJ�@A@@��@@�@@��@@e�@?خ@?K�@>�,@>a|@>�@=�~@<��@<~(@<[�@<1'@<x@;�@;)_@:��@:E�@9��@9�@9zx@9IR@9 \@8�|@8�@8[�@7�@7qv@7$t@6��@6�1@65?@6 �@5�C@5��@5`B@4�f@4��@4��@4��@3�q@3@O@3S@2��@2͟@2��@2Ta@2_@1�h@1(�@1;@0��@0�@0Ɇ@0��@0]d@0'R@01@/�}@/�@/U�@/(@.�'@.^5@.$�@-��@-�d@-��@-S&@,�v@,�p@,�@,�p@,��@,PH@,�@+�0@+��@+\)@*��@*�r@*0U@*�@)��@)�t@)��@)S&@(�P@(��@(�@'��@'6z@&�'@&�L@&�@&��@&��@&�r@&s�@&GE@&5?@%�z@%L�@$��@$�9@$�_@$U2@$>B@$2�@$,=@$  @#ݘ@#�K@#��@#Z�@#@O@"҉@"~�@"YK@"{@!�@!�~@!=�@!�@!V@ �v@ ��@ oi@ �@��@_p@�@��@��@Q@�@��@�o@��@�-@��@�@N<@�f@��@��@I�@:�@�@�@��@n/@g�@8@��@�@c @+k@	@��@�@��@��@c@s�@Y�@B�@�@�`@Ɇ@��@��@��@��@��@��@�:@��@qv@e�@,�@�@��@C�@�@�@�.@��@�9@@��@�@��@��@�@ �@�;@��@t�@P�@33@��@�@��@Ta@8�@{@�@�@�n@^�@A @+@�I@m�@c�@Q�@?�@<�@1'@7@�@�w@�	@J#@Y@�@�y@�B@��@ff@M�@Ov@�@�@�@�H@��@�~@m]@#�@�@�@�@�P@�@�[@�p@�@�@]d@?�@ �@@��@��@�[@RT@'�@�@@�@S@
��@
��@
a|@
&�@	�o@	ϫ@	��@	��@	��@	p�@	<6@	�@�P@��@�v@�E@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Aʻ0Aʾ�AʾwA���A��[A���A���A�ŢA��A��?A��mA���A�ƨA��A�ɆA��A��)A��dA���A���A��}A�ΥA��HA���A���A���A��[A��aA���A��A��,A��gAʥ�A�/A�>AÐbA�K�A�qAA�A���A���A��:A�bA���A��6A�e�A��A�@A���A�qA���A�iyA�GA�#:A�A�A��jA���A���A��A�:�A���Ay��Aq�jAnU2Aml�Ak��AiQ�AdخAc�Ac�KAaJ�A]�KAY�UAWv�AV��AVn�AU��AT)�AO�UALFAHE9AHbAGw2AEYKA?��A<8A;A�A9j�A:�A:�A9��A8@�A8��A8�+A8L�A8�A6;dA5��A5}�A3�hA2OvA1iDA0��A/ �A-oiA+�,A*&A)��A)~A(�)A(��A*�tA*��A*��A*�TA*~�A*��A*O�A)��A)�A)!�A'�sA%�A%��A%�A$�A$��A$
�A#��A#��A#?A!\�A u�A bA�$AXyA�Ae�A\�Av�A�A�oA��A+A�5A��AJ#A
=A��AZ�A[WA�:A2aAuA�A��AXyA%A��Ab�A+kA�+A��A@�A�=A2aA�"AیAcA֡A~AN<A�A��AYKA��AtTA�A��AiDAOA
�EA
p�A
,=A	��A	~AT�AE�Ax�A��Al�A:�A��A��A��A�A��A�SA�2AzAXyA`�A�A�4A"�A��A�A�\AC-A�A��A�HA��A��A
=A _�@��"@�ی@�ff@�e�@���@�T�@��@��@���@�ԕ@��F@�=@�8@�g8@�4@�o@��9@�/�@�@��/@�A�@���@�Ɇ@��3@�Y�@���@�@�@뽥@�"�@��s@�$@�(�@�A @�Q�@�1@�r@�O@��#@��@�`B@�F@�>B@��Q@�t�@�!�@�z@�=@�@@��@�-�@��@�'�@��D@ߩ�@�K�@�kQ@�rG@��B@�($@��@۶F@ۙ�@�K�@ڮ}@�~(@�V@��@ٿH@ٍP@�X@ص�@�u@ת�@�g�@֮}@չ�@�A�@��@��@Թ�@�.�@��@Ӊ7@�Vm@�*0@��@҇�@�$�@Ѳ-@я�@�s@��@Гu@�=q@�u@�k�@��`@�d�@�P�@�r�@���@��@���@���@�`B@���@�e�@���@��@��Q@�J#@��@Ƭ@�*�@şV@��@�[�@��+@�� @�t�@�;@«6@�g8@���@�l�@��H@�Z@�6@���@�|@�S�@�oi@��@�1@���@��@��@��p@�|�@��@�A�@��F@�e�@�I�@��@���@���@�#�@�ѷ@��@���@�=q@���@���@��@���@�g8@�8�@�{@�@�9�@��,@�l�@�@��@��Z@���@��-@���@�e,@��X@��Y@�&�@��+@�ݘ@���@��-@���@���@�e,@��E@��b@�u�@��@���@�J#@���@��@���@�A�@���@��X@�e�@��@�5?@�خ@�p�@��@��y@���@���@��b@�^5@��@�H�@���@�#:@��@�˒@��@��\@�K^@� �@��X@���@�H�@��Z@�x�@��H@���@�u%@�5?@�]�@��@���@��@��@@�s�@��@��@�I�@�_@��
@���@�w2@���@�I�@���@���@�iD@�W?@�A @�+@���@��U@�tT@�?�@��V@��X@���@���@��D@��+@��@�q@�\�@�Ft@�@��t@�>�@��@��f@���@��I@��@�]d@�&�@�@��&@��S@�;@��B@��@�p;@�!@�ԕ@���@���@�W?@��@��@���@���@�[�@�!�@���@���@���@��	@��@���@�M@��&@��"@�_p@�4@��@��L@�4n@��q@��@�/@���@�@��T@��V@���@�g�@�\)@�U�@�o@�ȴ@�{�@�Z@�b@���@���@�q�@�c @�GE@�4@��Z@�˒@��=@�a�@�0�@��B@��D@�A�@�  @��@�X@� i@��v@��L@�.�@�@��k@�(@�oi@��@���@�7L@���@�s�@��@��@\)@C@~�F@~B[@}��@|��@{��@{{J@z�@za|@z)�@yx�@y+@xK^@wU�@v��@v^5@v�@u��@uo @u�@tm�@s�@s@O@s�@r�<@ra|@q�@p�@p9X@o�@o��@oRT@o"�@n�@n��@nOv@n@m��@mhs@l_@k�	@kH�@j��@j��@j��@j�@j{�@jM�@j@i�@iw2@h��@hS�@h1@hG@g�Q@g{J@g@O@f��@f�@f��@f{�@f�@e�j@d�f@d[�@dN�@c��@cdZ@b�"@b��@b�@bn�@b6�@a��@`Ĝ@`��@`Q�@`2�@` �@`�@_��@_$t@^�H@^@�@]�h@]8�@]�@\�/@\N�@[�r@[+@Z��@ZO@Y�@Y:�@X��@X9X@W�@W�q@WH�@V��@V�@Uc�@U�@T�9@Th�@S�@So�@R�@Rq�@R@�@R.�@R@Q�@Q=�@QV@P�|@P�U@PH@O_p@Oo@N�@Nd�@N6�@M�o@M8�@MV@L�@L�@LK^@K�
@K_p@K!-@J�@J�F@J@I�@I/@Hی@H�@H��@HQ�@H6@H7@Ga@F�\@FL0@F@E�n@EY�@E�@D��@D�Y@Dh�@D~@Cy�@B��@Bl�@B
�@AJ�@A@@��@@�@@��@@e�@?خ@?K�@>�,@>a|@>�@=�~@<��@<~(@<[�@<1'@<x@;�@;)_@:��@:E�@9��@9�@9zx@9IR@9 \@8�|@8�@8[�@7�@7qv@7$t@6��@6�1@65?@6 �@5�C@5��@5`B@4�f@4��@4��@4��@3�q@3@O@3S@2��@2͟@2��@2Ta@2_@1�h@1(�@1;@0��@0�@0Ɇ@0��@0]d@0'R@01@/�}@/�@/U�@/(@.�'@.^5@.$�@-��@-�d@-��@-S&@,�v@,�p@,�@,�p@,��@,PH@,�@+�0@+��@+\)@*��@*�r@*0U@*�@)��@)�t@)��@)S&@(�P@(��@(�@'��@'6z@&�'@&�L@&�@&��@&��@&�r@&s�@&GE@&5?@%�z@%L�@$��@$�9@$�_@$U2@$>B@$2�@$,=@$  @#ݘ@#�K@#��@#Z�@#@O@"҉@"~�@"YK@"{@!�@!�~@!=�@!�@!V@ �v@ ��@ oi@ �@��@_p@�@��@��@Q@�@��@�o@��@�-@��@�@N<@�f@��@��@I�@:�@�@�@��@n/@g�@8@��@�@c @+k@	@��@�@��@��@c@s�@Y�@B�@�@�`@Ɇ@��@��@��@��@��@��@�:@��@qv@e�@,�@�@��@C�@�@�@�.@��@�9@@��@�@��@��@�@ �@�;@��@t�@P�@33@��@�@��@Ta@8�@{@�@�@�n@^�@A @+@�I@m�@c�@Q�@?�@<�@1'@7@�@�w@�	@J#@Y@�@�y@�B@��@ff@M�@Ov@�@�@�@�H@��@�~@m]@#�@�@�@�@�P@�@�[@�p@�@�@]d@?�@ �@@��@��@�[@RT@'�@�@@�@S@
��@
��@
a|@
&�@	�o@	ϫ@	��@	��@	��@	p�@	<6@	�@�P@��@�v@�E@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B�pB��B��B��B��B��B��B��B��B�VB�VB�pB�pB�pB�B�B�<B�pB�pB�<B�pB��B�pB��B��B��B��B	~B	qvB	�B	�)B	�B	�5B	�B	�B
�B
�B
�B0�B
��B
�;B
��B
�B
��B
��B
� B
oiB
D�B
7B
9B	��B	�B	��B	�B	��B	��B	��B	�.B	�EB	z�B	f�B	`BB	^B	Q4B	=�B	+�B	;B	�B	�B	�B	�B�BB�|B��B�B�tB�B��B�XB��B	�B	,�B	:xB	O�B	kkB	��B	�)B	��B	�7B	چB	�FB	�cB	�B	�B	��B	�B	�B	�B	��B	ݲB	��B	�VB	�wB	�0B
3�B
>wB
@4B
DB
D�B
GzB
E�B
B[B
G�B
F�B
CB
:^B
8RB
7�B
:B
:^B
:^B
:�B
>(B
=�B
4�B
,�B
*�B
)�B
'�B
$&B
 vB
�B
sB
�B
B
EB
B
;B
�B
�B
dB
�B
+B
�B
VB
B
B

#B
	B
_B
�B
�B
9B
gB
�B
�B
�B
 4B	��B	��B	�"B	�<B	�B	�LB	��B	�B	�wB	�qB	�0B	��B	��B	��B	�]B	��B	�=B	�B	�$B	�LB	��B	ޞB	��B	�yB	�OB	��B	��B	�vB	�B	�PB	�"B
 B
 4B
 OB	�(B
�B
B
	�B
�B
[B
oB
 �B	�wB	�wB	�PB	�<B	��B	�cB
 OB	��B	�lB	�B	�UB	�B	�B	��B
 �B
mB
oB	��B	�B	��B	�B	�%B	�B	�B	�}B	��B	�wB	�)B	�IB	�}B	�B	��B	��B	�B	�B	�B	��B	��B	�MB	�FB	�JB	��B	��B	�$B	��B	��B	��B
B
	7B
1B
�B
�B
�B
�B
�B
�B
�B
fB
KB
�B
�B
�B
B
B
�B
�B
AB
�B
�B
 �B
 �B
 B	��B
 OB	��B
B
B
 B
 �B
 �B
B
 B
 B
 B	��B
  B	��B	�}B	�B	��B	��B	�}B	��B	��B	��B
  B	��B
 B
 �B
 �B
 iB
 �B
 OB	��B	��B	�.B	�wB	�cB
 B
 OB
B
 4B
  B	�}B	��B	��B
 4B
�B
�B
B
-B
B
B
�B
3B
{B
�B
�B
�B
{B
�B
aB
{B
�B
�B
�B
B
�B
+B
�B
�B
B
tB
fB
B
�B
�B
�B
�B
�B
�B
EB
+B
+B
_B
�B
�B
B
�B

�B

�B

�B

rB

#B

	B
	�B
	�B
	�B
	RB
	B
	RB
	�B

#B

XB

=B

XB

XB

=B
�B
�B

�B
	�B

=B

	B

rB

�B
DB
JB
0B
0B
B

�B
B
�B
�B
�B
�B
"B
�B
�B
�B
B
vB
�B
�B
4B
�B
 B
�B
�B
4B
4B
�B
}B
}B
B
bB
B
}B
 B
�B
�B
�B
4B
:B
TB
:B
�B
�B
&B
B
B
�B
�B
�B
�B
B
�B
�B
MB
�B
�B
�B
�B
�B
�B
SB
�B
?B
?B
?B
?B
?B
sB
YB

B
$B
�B
�B
�B
�B
	B
=B
WB
=B
#B
WB
�B
)B
�B
�B
�B
B
B
B
/B
B
B
B
dB
B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
~B
�B
B
�B
IB
IB
B
B
�B
�B
 \B
�B
 \B
 'B
 BB
!B
!B
 �B
!HB
"�B
"�B
#:B
#nB
#�B
#�B
#�B
#:B
#�B
#�B
$&B
#�B
$@B
$�B
%�B
%�B
%�B
&2B
&fB
&�B
&�B
&�B
'B
'8B
'�B
'mB
'�B
'�B
'mB
(sB
(�B
)DB
)yB
)�B
*KB
+B
+kB
+B
*�B
+B
+B
+�B
+�B
,�B
-)B
-)B
-CB
-�B
-�B
.B
.�B
/5B
/�B
0B
0oB
0�B
1AB
1AB
2-B
3B
3hB
3�B
3�B
4B
4B
4nB
4�B
5�B
5�B
5�B
6+B
6FB
6zB
7fB
7�B
7�B
88B
8RB
8lB
8�B
8�B
8�B
9$B
9XB
9XB
:DB
:�B
:�B
;JB
;dB
;dB
;B
;B
;�B
;�B
;�B
;�B
<jB
<�B
<�B
<�B
<�B
<�B
<�B
="B
="B
=VB
=VB
=�B
=qB
>�B
>�B
>�B
>�B
?B
?cB
?cB
?}B
?�B
?�B
@4B
@�B
A;B
AoB
AoB
AoB
AoB
A�B
BB
B[B
C-B
C�B
DMB
DgB
DgB
EB
EB
E�B
E�B
E9B
EmB
FB
F?B
F�B
F�B
G_B
H�B
H�B
I�B
I�B
J#B
JXB
JrB
J�B
KxB
K�B
LJB
LJB
L0B
L0B
L~B
L~B
L~B
L~B
L~B
MB
M�B
M�B
M�B
NVB
N<B
NpB
OBB
O�B
O�B
O�B
PB
PB
PHB
P�B
Q B
QNB
Q�B
Q�B
Q�B
RB
RB
R�B
R�B
R�B
R�B
S[B
S�B
SuB
S�B
S�B
TFB
TaB
T�B
T�B
T�B
T�B
UMB
U�B
VB
VSB
W?B
WsB
WsB
WsB
W�B
W�B
X+B
X�B
YeB
Y�B
ZB
ZkB
[WB
[�B
[�B
[�B
[�B
\CB
\]B
\�B
]B
]IB
]dB
]�B
]�B
]�B
]�B
^B
^5B
^�B
^�B
_B
_�B
_�B
`B
_�B
`BB
`BB
`\B
`�B
`�B
`�B
`�B
a�B
a�B
bB
a�B
a�B
bNB
bNB
b�B
c B
cnB
cnB
cTB
cTB
c�B
c�B
c�B
c�B
c�B
d@B
d@B
d�B
d�B
e,B
ezB
e�B
e�B
e�B
e�B
fLB
f�B
f�B
f�B
f�B
f�B
f�B
gB
gmB
gmB
g�B
g�B
h>B
h�B
h�B
h�B
h�B
h�B
iB
iDB
iyB
i�B
i�B
i�B
i�B
jB
jB
j0B
jKB
jeB
jB
j0B
jB
jeB
j�B
k6B
k6B
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
lWB
l=B
l�B
l�B
l�B
mCB
mwB
m�B
m�B
n/B
n/B
ncB
ncB
n�B
o B
o5B
o�B
o�B
o�B
o�B
p;B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
qvB
qvB
q�B
q�B
q�B
rB
r�B
r�B
r�B
r�B
r�B
r�B
s3B
shB
s�B
s�B
s�B
s�B
s�B
tB
s�B
tB
tB
tB
t9B
tnB
tnB
tnB
t�B
uZB
uZB
utB
u�B
u�B
u�B
vB
vB
vFB
v+B
vzB
v�B
wB
w2B
w2B
wB
w2B
v�B
wB
w�B
w�B
w�B
xB
xRB
x�B
x�B
x�B
y$B
y>B
yXB
yrB
y�B
y�B
y�B
zB
zDB
zxB
z�B
z�B
z�B
{0B
{�B
|6B
|PB
|PB
|�B
|�B
|�B
}B
}"B
}�B
}�B
~B
~]B
~]B
~�B
~wB
~wB
~�B
~�B
B
}B
�B
�B
�B
� B
�B
�B
��B
��B
��B
��B
��B
�B
�B
��B
�B
� B
�;B
�UB
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�uB
��B
��B
��B
�aB
��B
��B
��B
�B
�MB
�SB
�B
�?B
�?B
�?B
�%B
�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B�pB��B��B��B��B��B��B��B��B�VB�VB�pB�pB�pB�B�B�<B�pB�pB�<B�pB��B�pB��B��B��B��B	~B	qvB	�B	�)B	�B	�5B	�B	�B
�B
�B
�B0�B
��B
�;B
��B
�B
��B
��B
� B
oiB
D�B
7B
9B	��B	�B	��B	�B	��B	��B	��B	�.B	�EB	z�B	f�B	`BB	^B	Q4B	=�B	+�B	;B	�B	�B	�B	�B�BB�|B��B�B�tB�B��B�XB��B	�B	,�B	:xB	O�B	kkB	��B	�)B	��B	�7B	چB	�FB	�cB	�B	�B	��B	�B	�B	�B	��B	ݲB	��B	�VB	�wB	�0B
3�B
>wB
@4B
DB
D�B
GzB
E�B
B[B
G�B
F�B
CB
:^B
8RB
7�B
:B
:^B
:^B
:�B
>(B
=�B
4�B
,�B
*�B
)�B
'�B
$&B
 vB
�B
sB
�B
B
EB
B
;B
�B
�B
dB
�B
+B
�B
VB
B
B

#B
	B
_B
�B
�B
9B
gB
�B
�B
�B
 4B	��B	��B	�"B	�<B	�B	�LB	��B	�B	�wB	�qB	�0B	��B	��B	��B	�]B	��B	�=B	�B	�$B	�LB	��B	ޞB	��B	�yB	�OB	��B	��B	�vB	�B	�PB	�"B
 B
 4B
 OB	�(B
�B
B
	�B
�B
[B
oB
 �B	�wB	�wB	�PB	�<B	��B	�cB
 OB	��B	�lB	�B	�UB	�B	�B	��B
 �B
mB
oB	��B	�B	��B	�B	�%B	�B	�B	�}B	��B	�wB	�)B	�IB	�}B	�B	��B	��B	�B	�B	�B	��B	��B	�MB	�FB	�JB	��B	��B	�$B	��B	��B	��B
B
	7B
1B
�B
�B
�B
�B
�B
�B
�B
fB
KB
�B
�B
�B
B
B
�B
�B
AB
�B
�B
 �B
 �B
 B	��B
 OB	��B
B
B
 B
 �B
 �B
B
 B
 B
 B	��B
  B	��B	�}B	�B	��B	��B	�}B	��B	��B	��B
  B	��B
 B
 �B
 �B
 iB
 �B
 OB	��B	��B	�.B	�wB	�cB
 B
 OB
B
 4B
  B	�}B	��B	��B
 4B
�B
�B
B
-B
B
B
�B
3B
{B
�B
�B
�B
{B
�B
aB
{B
�B
�B
�B
B
�B
+B
�B
�B
B
tB
fB
B
�B
�B
�B
�B
�B
�B
EB
+B
+B
_B
�B
�B
B
�B

�B

�B

�B

rB

#B

	B
	�B
	�B
	�B
	RB
	B
	RB
	�B

#B

XB

=B

XB

XB

=B
�B
�B

�B
	�B

=B

	B

rB

�B
DB
JB
0B
0B
B

�B
B
�B
�B
�B
�B
"B
�B
�B
�B
B
vB
�B
�B
4B
�B
 B
�B
�B
4B
4B
�B
}B
}B
B
bB
B
}B
 B
�B
�B
�B
4B
:B
TB
:B
�B
�B
&B
B
B
�B
�B
�B
�B
B
�B
�B
MB
�B
�B
�B
�B
�B
�B
SB
�B
?B
?B
?B
?B
?B
sB
YB

B
$B
�B
�B
�B
�B
	B
=B
WB
=B
#B
WB
�B
)B
�B
�B
�B
B
B
B
/B
B
B
B
dB
B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
~B
�B
B
�B
IB
IB
B
B
�B
�B
 \B
�B
 \B
 'B
 BB
!B
!B
 �B
!HB
"�B
"�B
#:B
#nB
#�B
#�B
#�B
#:B
#�B
#�B
$&B
#�B
$@B
$�B
%�B
%�B
%�B
&2B
&fB
&�B
&�B
&�B
'B
'8B
'�B
'mB
'�B
'�B
'mB
(sB
(�B
)DB
)yB
)�B
*KB
+B
+kB
+B
*�B
+B
+B
+�B
+�B
,�B
-)B
-)B
-CB
-�B
-�B
.B
.�B
/5B
/�B
0B
0oB
0�B
1AB
1AB
2-B
3B
3hB
3�B
3�B
4B
4B
4nB
4�B
5�B
5�B
5�B
6+B
6FB
6zB
7fB
7�B
7�B
88B
8RB
8lB
8�B
8�B
8�B
9$B
9XB
9XB
:DB
:�B
:�B
;JB
;dB
;dB
;B
;B
;�B
;�B
;�B
;�B
<jB
<�B
<�B
<�B
<�B
<�B
<�B
="B
="B
=VB
=VB
=�B
=qB
>�B
>�B
>�B
>�B
?B
?cB
?cB
?}B
?�B
?�B
@4B
@�B
A;B
AoB
AoB
AoB
AoB
A�B
BB
B[B
C-B
C�B
DMB
DgB
DgB
EB
EB
E�B
E�B
E9B
EmB
FB
F?B
F�B
F�B
G_B
H�B
H�B
I�B
I�B
J#B
JXB
JrB
J�B
KxB
K�B
LJB
LJB
L0B
L0B
L~B
L~B
L~B
L~B
L~B
MB
M�B
M�B
M�B
NVB
N<B
NpB
OBB
O�B
O�B
O�B
PB
PB
PHB
P�B
Q B
QNB
Q�B
Q�B
Q�B
RB
RB
R�B
R�B
R�B
R�B
S[B
S�B
SuB
S�B
S�B
TFB
TaB
T�B
T�B
T�B
T�B
UMB
U�B
VB
VSB
W?B
WsB
WsB
WsB
W�B
W�B
X+B
X�B
YeB
Y�B
ZB
ZkB
[WB
[�B
[�B
[�B
[�B
\CB
\]B
\�B
]B
]IB
]dB
]�B
]�B
]�B
]�B
^B
^5B
^�B
^�B
_B
_�B
_�B
`B
_�B
`BB
`BB
`\B
`�B
`�B
`�B
`�B
a�B
a�B
bB
a�B
a�B
bNB
bNB
b�B
c B
cnB
cnB
cTB
cTB
c�B
c�B
c�B
c�B
c�B
d@B
d@B
d�B
d�B
e,B
ezB
e�B
e�B
e�B
e�B
fLB
f�B
f�B
f�B
f�B
f�B
f�B
gB
gmB
gmB
g�B
g�B
h>B
h�B
h�B
h�B
h�B
h�B
iB
iDB
iyB
i�B
i�B
i�B
i�B
jB
jB
j0B
jKB
jeB
jB
j0B
jB
jeB
j�B
k6B
k6B
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
lWB
l=B
l�B
l�B
l�B
mCB
mwB
m�B
m�B
n/B
n/B
ncB
ncB
n�B
o B
o5B
o�B
o�B
o�B
o�B
p;B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
qvB
qvB
q�B
q�B
q�B
rB
r�B
r�B
r�B
r�B
r�B
r�B
s3B
shB
s�B
s�B
s�B
s�B
s�B
tB
s�B
tB
tB
tB
t9B
tnB
tnB
tnB
t�B
uZB
uZB
utB
u�B
u�B
u�B
vB
vB
vFB
v+B
vzB
v�B
wB
w2B
w2B
wB
w2B
v�B
wB
w�B
w�B
w�B
xB
xRB
x�B
x�B
x�B
y$B
y>B
yXB
yrB
y�B
y�B
y�B
zB
zDB
zxB
z�B
z�B
z�B
{0B
{�B
|6B
|PB
|PB
|�B
|�B
|�B
}B
}"B
}�B
}�B
~B
~]B
~]B
~�B
~wB
~wB
~�B
~�B
B
}B
�B
�B
�B
� B
�B
�B
��B
��B
��B
��B
��B
�B
�B
��B
�B
� B
�;B
�UB
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�uB
��B
��B
��B
�aB
��B
��B
��B
�B
�MB
�SB
�B
�?B
�?B
�?B
�%B
�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105243  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192341  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192342  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192342                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042349  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042349  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                