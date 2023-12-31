CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-11-03T00:35:16Z creation;2017-11-03T00:35:20Z conversion to V3.1;2019-12-19T07:57:42Z update;     
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
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20171103003516  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_175                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�2u��1   @�2v}'Ҁ@;$��q�j�dq��,1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dry�Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
B��
B�
=B��
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
B��
B��
C�C�CCC	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D�{D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drt{Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D��qD�=qD�}qD��qD��=D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD� �D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�z=D��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��=D�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��TA��#A��
A���A���A���A���A���A���A���A���A���A��AȲ-Aț�AȃA�bNA��A�^5A��/A�p�A�`BA�jA�jA�p�AƁAƓuAƝ�Aƣ�AƬA���A��
A�v�A�l�A�bA��A�7LA�bNA�;dA���A��!A�%A���A�~�A���A��
A���A��A�33A���A��DA�ȴA��
A�1'A�K�A��uA�1'A�E�A�/A��!A���A�A��`A�r�A���A��\A�ffA���A��A���A�&�A�dZA���A�ZA��A���A��yA�XA��RA�S�A���A�Q�A���A�&�A��A��A��;A��A�A�VA�9XAS�A~VA|��A{�A{XAy�Ay/Ax5?Aw&�Av��At�Ar-Ap�Ao��Ao�An�9An �Am��Al�`Al=qAkt�Aj5?AiS�Ah�`Ag�mAfr�AeƨAdbNAb��Aa�A`��A_��A_S�A^�A^r�A]��A]`BA]G�A]/A\��A\��A\�RA\z�A\  A[�AZ�AZI�AXr�AVjAT�`ASdZAQdZAP��AO�PAN��AN�9ANAM|�AL�AK��AH9XAG+AG"�AG&�AG&�AG"�AG%AF�AD�ACdZAB��AB��AB�RAB �AAp�A@ZA>��A>ĜA>n�A=�;A<��A<bA;l�A:�jA9��A8=qA7hsA7?}A6��A6n�A5��A4VA2ffA1��A1+A0��A0M�A/dZA.�DA-�A-\)A,9XA*�A)��A(Q�A'p�A&��A%�wA$I�A#33A"z�A!��A��AS�A��A$�A7LAbA�TA%Ap�AE�A�AXA��A��AXA�/A�\A �A��A��AS�A/A�AE�A�
A�A
=A��A
r�A
1'A	�A�/AM�A��A�A?}A��AM�A�A��A
=A��A��AS�A��AA�A��AG�A �@���@��\@��@��#@�9X@��@���@���@��;@��@�
=@�-@�Ĝ@� �@��@��
@�7L@�Q�@��
@�dZ@�
=@�R@�\@�=q@�^@��@�\)@⟾@�@�7L@��@�33@��#@�(�@��T@���@�@�V@�@���@�A�@��@��@ёh@�r�@Ϯ@���@���@��@��`@�b@���@˕�@�t�@�
=@��y@ʧ�@ʗ�@ʗ�@ʗ�@ʗ�@�~�@�n�@�^5@�M�@�$�@ɩ�@�K�@�\)@§�@�5?@��7@� �@��@�n�@��@�A�@���@���@�p�@�I�@�dZ@��@�$�@�X@�z�@��F@���@�=q@�@�Z@��+@��9@���@��w@�S�@�`B@���@�9X@�l�@�"�@���@��H@�r�@���@�;d@�v�@��@��h@��/@�Z@�A�@�(�@�bN@�I�@��u@�9X@��w@�b@��@��7@��\@�x�@� �@��\@��@��P@�\)@�+@���@�^5@��h@��#@��^@��@���@��@�=q@���@��-@�hs@���@�z�@��@�I�@��@�  @�Q�@���@�G�@�Q�@�b@�dZ@�@�n�@��#@�J@�ȴ@��R@�@��+@���@�5?@��@��R@�G�@�r�@�r�@�r�@�j@� �@��w@��@�l�@�C�@��@�v�@�-@���@��7@�`B@�V@���@���@�Z@���@���@�33@�@���@��H@���@���@���@�~�@�V@�5?@��@���@��@�`B@�/@��@�I�@�A�@�1'@�w@~�y@~5?@}�@{�m@{�F@{"�@z�\@z=q@y��@yX@y�@x��@x�9@xbN@x �@w��@v�R@v��@vE�@u�@u��@t�/@s��@s�
@s"�@r~�@rM�@r�@q��@qX@p��@pQ�@o�@o�P@o
=@nv�@nV@n$�@n@m��@m�h@m?}@l��@lj@l1@kƨ@k��@kt�@k33@j��@i�^@iX@iG�@i7L@i7L@i7L@i7L@i�@i%@h��@i%@i�@h��@h �@g|�@gl�@g\)@f�@fv�@fE�@f{@f@e�@e@e��@ep�@e?}@d�/@d�D@c�
@c��@ct�@co@b��@b��@b��@b^5@a�@a�7@aG�@`�`@`�`@`��@`�@`bN@`A�@`  @_��@_�P@_|�@_|�@_+@^�R@^��@]�@]p�@[�
@["�@Z�@Z�@Z�H@Z��@Z=q@Y�^@YX@YX@YX@Y7L@Y%@X��@X��@XbN@W�;@Wl�@Vff@U��@U/@UV@U�@UO�@U�@U�-@Up�@UO�@U�T@V��@V��@V5?@V@U�@V@U��@T�/@S�F@R�H@R��@S@S��@Sƨ@SC�@Q��@Q7L@QG�@Q�7@Qx�@Q&�@P��@PĜ@P�@P1'@P �@P  @O�P@OK�@N�@N�@NV@M@M?}@M�@L�/@L�/@L��@L�D@LZ@L(�@K��@K��@Kt�@K33@Ko@K@J�@J��@JM�@I�#@I��@IX@I7L@I&�@H��@H�`@H�@H  @G�w@G\)@Fv�@E��@D�/@D�D@D(�@C��@C�@CS�@B��@B^5@Ahs@@��@@�9@@r�@@bN@@Q�@?�@>��@>��@>E�@=�@>@=@=��@=�T@=�h@=/@<j@<�@;�
@:��@:-@:J@9�^@9�7@9%@8�@81'@8b@7�;@7��@7�P@6�@6v�@6{@5�T@5�@4�@4�@4�D@4z�@4I�@49X@3��@3��@2�@2�!@2-@2J@1��@1��@1hs@1%@1%@1%@0�`@0Ĝ@0�@/��@/�P@/l�@/l�@/l�@/\)@.��@.�@.��@.v�@.V@.V@.V@.V@.@-�T@-��@-��@-��@-�@-�@-�@-�@-�h@-�@-p�@-O�@-?}@-/@-/@-�@,��@,�D@,�D@,�D@,9X@,1@+�
@+�F@+dZ@+33@+@*�@*�H@*�!@*M�@*J@)��@)x�@(�9@'�w@';d@&�R@&�+@&ff@&$�@%`B@%�@$�@$�@$Z@$Z@#��@#�F@#dZ@#C�@#33@#o@#@"�@"��@"M�@"=q@"-@"�@"�@!��@!�@!��@!x�@!&�@!�@ ��@ ��@ ��@ �`@ ��@ ��@ r�@ A�@�;@|�@l�@\)@\)@\)@\)@l�@l�@\)@K�@�@�+@@@�-@��@��@�@�@�@�@p�@`B@�@�/@Z@�
@t�@dZ@C�@@�!@=q@J@��@�7@7L@�@�@Q�@ �@�;@|�@+@��@�@��@�+@ff@@`B@O�@?}@�@�j@z�@1@��@��@�m@�F@�@t�@C�@o@��@��@�\@n�@�@J@�^@X@&�@��@��@Ĝ@��@bN@ �@�@�;@�;@��@�w@��@|�@+@�@v�@V@E�@E�@E�@E�@5?@5?@5?@$�@{@�@�h@O�@V@��@��@z�@�@�
@��@dZ@C�@"�@
�@o@@o@@
��@
��@
�\@
~�@
^5@
M�@
�@	x�@	G�@	7L@	&�@	�@��@��@�9@A�@b@�@  @�@�w@��@|�@K�@+@�@��@ȴ@�+@ff@V@E�@5?@{@@�T@@p�@V@��@�@�/@��@��@9X@1@��@�m@�
@��@t�@S�@"�@�H@��@��@^5@=q@�@�@�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��TA��#A��
A���A���A���A���A���A���A���A���A���A��AȲ-Aț�AȃA�bNA��A�^5A��/A�p�A�`BA�jA�jA�p�AƁAƓuAƝ�Aƣ�AƬA���A��
A�v�A�l�A�bA��A�7LA�bNA�;dA���A��!A�%A���A�~�A���A��
A���A��A�33A���A��DA�ȴA��
A�1'A�K�A��uA�1'A�E�A�/A��!A���A�A��`A�r�A���A��\A�ffA���A��A���A�&�A�dZA���A�ZA��A���A��yA�XA��RA�S�A���A�Q�A���A�&�A��A��A��;A��A�A�VA�9XAS�A~VA|��A{�A{XAy�Ay/Ax5?Aw&�Av��At�Ar-Ap�Ao��Ao�An�9An �Am��Al�`Al=qAkt�Aj5?AiS�Ah�`Ag�mAfr�AeƨAdbNAb��Aa�A`��A_��A_S�A^�A^r�A]��A]`BA]G�A]/A\��A\��A\�RA\z�A\  A[�AZ�AZI�AXr�AVjAT�`ASdZAQdZAP��AO�PAN��AN�9ANAM|�AL�AK��AH9XAG+AG"�AG&�AG&�AG"�AG%AF�AD�ACdZAB��AB��AB�RAB �AAp�A@ZA>��A>ĜA>n�A=�;A<��A<bA;l�A:�jA9��A8=qA7hsA7?}A6��A6n�A5��A4VA2ffA1��A1+A0��A0M�A/dZA.�DA-�A-\)A,9XA*�A)��A(Q�A'p�A&��A%�wA$I�A#33A"z�A!��A��AS�A��A$�A7LAbA�TA%Ap�AE�A�AXA��A��AXA�/A�\A �A��A��AS�A/A�AE�A�
A�A
=A��A
r�A
1'A	�A�/AM�A��A�A?}A��AM�A�A��A
=A��A��AS�A��AA�A��AG�A �@���@��\@��@��#@�9X@��@���@���@��;@��@�
=@�-@�Ĝ@� �@��@��
@�7L@�Q�@��
@�dZ@�
=@�R@�\@�=q@�^@��@�\)@⟾@�@�7L@��@�33@��#@�(�@��T@���@�@�V@�@���@�A�@��@��@ёh@�r�@Ϯ@���@���@��@��`@�b@���@˕�@�t�@�
=@��y@ʧ�@ʗ�@ʗ�@ʗ�@ʗ�@�~�@�n�@�^5@�M�@�$�@ɩ�@�K�@�\)@§�@�5?@��7@� �@��@�n�@��@�A�@���@���@�p�@�I�@�dZ@��@�$�@�X@�z�@��F@���@�=q@�@�Z@��+@��9@���@��w@�S�@�`B@���@�9X@�l�@�"�@���@��H@�r�@���@�;d@�v�@��@��h@��/@�Z@�A�@�(�@�bN@�I�@��u@�9X@��w@�b@��@��7@��\@�x�@� �@��\@��@��P@�\)@�+@���@�^5@��h@��#@��^@��@���@��@�=q@���@��-@�hs@���@�z�@��@�I�@��@�  @�Q�@���@�G�@�Q�@�b@�dZ@�@�n�@��#@�J@�ȴ@��R@�@��+@���@�5?@��@��R@�G�@�r�@�r�@�r�@�j@� �@��w@��@�l�@�C�@��@�v�@�-@���@��7@�`B@�V@���@���@�Z@���@���@�33@�@���@��H@���@���@���@�~�@�V@�5?@��@���@��@�`B@�/@��@�I�@�A�@�1'@�w@~�y@~5?@}�@{�m@{�F@{"�@z�\@z=q@y��@yX@y�@x��@x�9@xbN@x �@w��@v�R@v��@vE�@u�@u��@t�/@s��@s�
@s"�@r~�@rM�@r�@q��@qX@p��@pQ�@o�@o�P@o
=@nv�@nV@n$�@n@m��@m�h@m?}@l��@lj@l1@kƨ@k��@kt�@k33@j��@i�^@iX@iG�@i7L@i7L@i7L@i7L@i�@i%@h��@i%@i�@h��@h �@g|�@gl�@g\)@f�@fv�@fE�@f{@f@e�@e@e��@ep�@e?}@d�/@d�D@c�
@c��@ct�@co@b��@b��@b��@b^5@a�@a�7@aG�@`�`@`�`@`��@`�@`bN@`A�@`  @_��@_�P@_|�@_|�@_+@^�R@^��@]�@]p�@[�
@["�@Z�@Z�@Z�H@Z��@Z=q@Y�^@YX@YX@YX@Y7L@Y%@X��@X��@XbN@W�;@Wl�@Vff@U��@U/@UV@U�@UO�@U�@U�-@Up�@UO�@U�T@V��@V��@V5?@V@U�@V@U��@T�/@S�F@R�H@R��@S@S��@Sƨ@SC�@Q��@Q7L@QG�@Q�7@Qx�@Q&�@P��@PĜ@P�@P1'@P �@P  @O�P@OK�@N�@N�@NV@M@M?}@M�@L�/@L�/@L��@L�D@LZ@L(�@K��@K��@Kt�@K33@Ko@K@J�@J��@JM�@I�#@I��@IX@I7L@I&�@H��@H�`@H�@H  @G�w@G\)@Fv�@E��@D�/@D�D@D(�@C��@C�@CS�@B��@B^5@Ahs@@��@@�9@@r�@@bN@@Q�@?�@>��@>��@>E�@=�@>@=@=��@=�T@=�h@=/@<j@<�@;�
@:��@:-@:J@9�^@9�7@9%@8�@81'@8b@7�;@7��@7�P@6�@6v�@6{@5�T@5�@4�@4�@4�D@4z�@4I�@49X@3��@3��@2�@2�!@2-@2J@1��@1��@1hs@1%@1%@1%@0�`@0Ĝ@0�@/��@/�P@/l�@/l�@/l�@/\)@.��@.�@.��@.v�@.V@.V@.V@.V@.@-�T@-��@-��@-��@-�@-�@-�@-�@-�h@-�@-p�@-O�@-?}@-/@-/@-�@,��@,�D@,�D@,�D@,9X@,1@+�
@+�F@+dZ@+33@+@*�@*�H@*�!@*M�@*J@)��@)x�@(�9@'�w@';d@&�R@&�+@&ff@&$�@%`B@%�@$�@$�@$Z@$Z@#��@#�F@#dZ@#C�@#33@#o@#@"�@"��@"M�@"=q@"-@"�@"�@!��@!�@!��@!x�@!&�@!�@ ��@ ��@ ��@ �`@ ��@ ��@ r�@ A�@�;@|�@l�@\)@\)@\)@\)@l�@l�@\)@K�@�@�+@@@�-@��@��@�@�@�@�@p�@`B@�@�/@Z@�
@t�@dZ@C�@@�!@=q@J@��@�7@7L@�@�@Q�@ �@�;@|�@+@��@�@��@�+@ff@@`B@O�@?}@�@�j@z�@1@��@��@�m@�F@�@t�@C�@o@��@��@�\@n�@�@J@�^@X@&�@��@��@Ĝ@��@bN@ �@�@�;@�;@��@�w@��@|�@+@�@v�@V@E�@E�@E�@E�@5?@5?@5?@$�@{@�@�h@O�@V@��@��@z�@�@�
@��@dZ@C�@"�@
�@o@@o@@
��@
��@
�\@
~�@
^5@
M�@
�@	x�@	G�@	7L@	&�@	�@��@��@�9@A�@b@�@  @�@�w@��@|�@K�@+@�@��@ȴ@�+@ff@V@E�@5?@{@@�T@@p�@V@��@�@�/@��@��@9X@1@��@�m@�
@��@t�@S�@"�@�H@��@��@^5@=q@�@�@�@�@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��BBÖBĜBĜBŢBŢBŢBŢBƨBȴB�
BuB)�B/B2-B2-B#�BVBB%BPB�B�B!�B&�B+B-B.B1'BE�B@�B'�B	7B�BB��B� BP�B�B6FB5?B&�BbB�sB�B�B��B��B��B�B�)B�B��BȴB�RB��B��B��B�bB�JB� Bm�Bs�Bp�Bq�BiyBT�BQ�BI�B>wB,B!�BuBB
��B
��B
�yB
�#B
��B
��B
ɺB
��B
�LB
�B
�B
�B
��B
��B
�VB
�+B
�B
w�B
s�B
p�B
gmB
cTB
^5B
S�B
N�B
A�B
.B
+B
$�B
#�B
"�B
�B
�B
uB
VB
1B
B	��B	��B	�B	�fB	�`B	�#B	��B	��B	ɺB	ǮB	ŢB	ŢB	B	�}B	�wB	��B	��B	�}B	�wB	�qB	�^B	�?B	�B	�B	��B	��B	�VB	�B	|�B	p�B	s�B	l�B	k�B	k�B	dZB	`BB	VB	N�B	9XB	@�B	G�B	G�B	F�B	D�B	A�B	;dB	.B	)�B	0!B	0!B	.B	'�B	!�B	�B	�B	�B	�B	{B	PB	bB	JB	DB	B��B��B��B��B��B�B�B�BB�BB�)B�#B�B��B��B��BɺBB�XB�?B�B�B�B��B��B��B��B�\B� B}�B�+B�1B�B~�B|�Bt�BjBk�Bo�Bl�BiyBe`BgmBe`BdZBbNBdZBbNB`BB_;BZBW
BR�BI�B;dBA�B<jBH�BE�BD�BF�BG�BF�BG�BF�BB�BD�BA�B@�B?}B=qB<jB:^B;dB9XB9XB33B;dB7LB33B0!B0!B/B0!B/B1'B2-B0!B+B+B)�B#�B �B!�B(�B+B)�B)�B(�B(�B&�B$�B$�B#�B(�B(�B(�B%�B �B�B�B�B&�B'�B.B.B.B/B/B2-B5?B33B33B2-B49B5?B9XB8RB;dB=qB>wB=qB?}B?}BA�BB�BB�BB�BB�BB�BB�BA�B?}B;dB5?B-B=qB>wB;dB7LB-B33B9XBA�B>wB?}BB�B@�BA�BD�BD�BE�BE�BH�BH�BJ�BL�BJ�BM�BQ�B[#B^5B]/BXB`BBbNBcTBffBgmBiyBjBjBm�Bl�Bq�Bq�Br�By�B|�B~�B� B�=B��B��B��B��B��B��B�{B��B�B��B��BB��B�wB�^B�B�dBȴBɺBɺB��B��B�
B�B�
B�B�
B�
B�B�;B�TB�HB�ZB�yB�mB�B�B�yB�mB�yB�B��B��B��B	  B	B	
=B	+B	B��B��B��B��B	  B��B��B��B��B��B��B��B��B��B	B	B	B	B	1B	DB	DB	JB	\B	uB	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	$�B	&�B	)�B	-B	-B	-B	0!B	6FB	9XB	9XB	=qB	=qB	>wB	=qB	D�B	D�B	F�B	I�B	J�B	K�B	N�B	O�B	P�B	P�B	Q�B	R�B	S�B	YB	YB	ZB	[#B	ZB	]/B	bNB	aHB	dZB	gmB	gmB	hsB	hsB	jB	m�B	o�B	q�B	s�B	u�B	y�B	z�B	{�B	|�B	}�B	~�B	�B	�B	�B	�B	�%B	�%B	�+B	�=B	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�!B	�3B	�?B	�RB	�XB	�XB	�^B	�jB	�jB	�dB	�jB	�wB	�}B	��B	B	ÖB	B	ĜB	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ƨB	ƨB	ȴB	ǮB	ȴB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�)B	�5B	�5B	�;B	�BB	�NB	�TB	�`B	�`B	�fB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
B
B
B
B
%B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B

=B
1B
	7B
1B
+B
1B
+B
1B
1B
1B
	7B
1B
+B
%B
B
%B
+B
+B
+B
+B
%B
%B
1B
1B
DB
PB
PB
\B
bB
\B
bB
\B
bB
bB
VB
\B
oB
oB
oB
oB
oB
{B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
 �B
!�B
#�B
#�B
#�B
#�B
#�B
"�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
(�B
+B
,B
,B
,B
-B
/B
0!B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
33B
2-B
2-B
33B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
6FB
8RB
7LB
8RB
7LB
7LB
;dB
<jB
=qB
>wB
=qB
<jB
?}B
?}B
?}B
@�B
A�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
H�B
G�B
H�B
H�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
K�B
J�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
N�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
S�B
R�B
R�B
R�B
T�B
T�B
T�B
VB
VB
VB
YB
YB
XB
XB
XB
XB
W
B
W
B
W
B
XB
XB
XB
XB
YB
XB
YB
YB
ZB
ZB
[#B
ZB
ZB
[#B
\)B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
dZB
e`B
e`B
e`B
dZB
dZB
dZB
dZB
e`B
ffB
ffB
ffB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��BBÖBĜBĜBŢBŢBŢBżBƎBȴB��B�B*0B/iB2�B2�B%FB\B�BYBPB�B�B!�B&�B*�B,�B-�B1'BFYBB�B-wB�B�|BǮB��B��B\CB'�B7�B6�B*KB�B�cB�B�B�8B�8B�DB�qB�OB��B�2B��B��B��B�B�5B��B��B��Br-Bt�Bq�Br�Bk�BXBS[BKB@�B/B#�BmB�B
�B
�zB
�B
�jB
��B
�B
��B
�[B
��B
�;B
�OB
�CB
��B
�dB
�HB
��B
�aB
y�B
t�B
q�B
iB
dZB
_pB
UMB
O�B
C�B
1AB
,�B
&2B
$tB
#TB
�B
=B
�B
\B
	RB
uB	�B	��B	�B	�>B	�B	��B	өB	�4B	�)B	ȴB	�tB	�%B	�GB	�4B	��B	��B	��B	��B	��B	��B	��B	��B	�;B	��B	��B	�B	��B	�B	~�B	sB	t�B	m�B	l=B	lB	eFB	a-B	W�B	P�B	=B	A�B	G�B	G�B	F�B	D�B	BB	<�B	0!B	+�B	0�B	0�B	.}B	(�B	#B	B	B	B	1B	�B	�B	hB	PB	JB	�B��B��B�PB�xB��B��B�wB�B�B��B��B��B�MB� B��B��B�3B�B��B�B�;B�)B��B��B�B��B�B�GB�B�B�B�[B�iB}�Bv+Bl�Bl�Bp;Bm�Bj�Bf�Bh
BfBd�BcBd�Bb�B`�B_�BZ�BW�BS�BK^B=�BCGB>BBIRBF�BE�BGzBHKBG_BH1BGEBC�BEBB[BAoB@OB>wB=qB;dB<6B:DB:^B4�B;�B8�B4�B1�B1AB0!B0�B0B1�B2�B0�B,"B+�B*�B%zB"hB#�B)�B+kB*eB*eB)DB)DB'mB%�B%�B$�B)�B)�B)yB&�B!�B�B!-BOB'�B)*B.�B.�B.�B/�B0!B2�B5�B4B3�B2�B4�B5�B9�B8�B;�B=�B>�B=�B?�B?�BA�BB�BB�BB�BB�BB�BB�BA�B?�B<PB72B/5B=�B?B<6B8�B/iB4�B:�BB'B?cB@4BC-BAoBB[BE9BE9BF?BFtBIlBIlBK^BMjBK�BOBBS&B[�B^�B]�BYB`�Bb�Bc�Bf�Bg�Bi�Bk�Bk6BnBm)BrBr-Bs3Bz*B}B.B� B��B��B�B�@B�B��B��B�SB��B��B��B�B��B��B��B�0B��B��BȴB��B�	B��B��B�
B�_B�sB�SB׍B�YBچB�!B�TB��B�B�B�B�"B��B�B��B��B�B��B��B�BB	 �B	�B	
=B	�B	[B�^B��B�XB�B	 B�B�BB�]B�B�.B�BB�VB�PB�HB	�B	3B	aB	mB	fB	xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"B	# B	$&B	%FB	'8B	*KB	-CB	-CB	-�B	0�B	6zB	9�B	9�B	=�B	=�B	>�B	>(B	D�B	D�B	F�B	J	B	KB	K�B	OB	O�B	Q B	QB	R B	S&B	TaB	Y1B	YKB	ZQB	[WB	Z�B	]�B	bhB	a�B	d�B	g�B	g�B	h�B	h�B	j�B	m�B	o�B	q�B	s�B	u�B	y�B	{B	|B	}B	~B	HB	�[B	�GB	�MB	�9B	�?B	�?B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�&B	�2B	�*B	�6B	�CB	�IB	�OB	�;B	�;B	�[B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ªB	ðB	��B	ĶB	ĶB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�KB	�B	� B	�B	�B	�B	� B	�FB	�B	�B	�+B	�+B	�+B	�KB	�7B	�kB	�]B	�jB	޸B	ߊB	��B	�NB	�nB	�`B	�zB	�B	�B	�mB	�KB	�WB	��B	��B	�B	��B	��B	��B	�-B	�B	��B	�B	��B	��B	��B	�B	�+B	��B	��B	��B	�B	�B	�B
 4B
 4B
 B
;B
 B
 4B
 B
AB
-B
aB
[B
GB
9B
?B
?B
?B
YB
SB
SB
SB
SB
YB
EB
fB
fB
KB
fB
fB
fB
	RB
	lB

XB

XB

rB

rB

rB
fB
	RB
fB
�B
�B
zB
fB
fB
fB
	lB
KB
_B
YB
�B
tB
_B
_B
_B
_B
�B
�B
KB
�B
^B
jB
�B
vB
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
!�B
!B
!�B
#�B
#�B
$B
$B
$B
#:B
&B
'B
&�B
&�B
'B
'B
(
B
(
B
(
B
)*B
*B
*B
)�B
)B
+B
,B
,B
,"B
-CB
/B
0!B
0;B
1'B
1'B
1'B
1[B
2GB
2-B
3MB
2GB
2aB
3MB
4TB
4TB
4TB
4TB
5ZB
5ZB
5ZB
6`B
6`B
7LB
7�B
7fB
6zB
8lB
7�B
8lB
7�B
7�B
;�B
<�B
=�B
>�B
=�B
<�B
?�B
?�B
?�B
@�B
A�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
H�B
G�B
H�B
H�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
K�B
J�B
LB
L�B
M�B
M�B
NB
M�B
N"B
NB
N�B
N�B
OB
O�B
O(B
Q B
Q B
Q B
RB
RB
SB
SB
S&B
TB
S&B
S&B
S&B
UB
UB
U2B
VB
VB
VSB
YB
Y1B
XEB
X+B
X+B
XB
W?B
W?B
W?B
X+B
X+B
X+B
X+B
Y1B
XEB
Y1B
YKB
Z7B
Z7B
[#B
Z7B
Z7B
[=B
\]B
]/B
]/B
^OB
^OB
^jB
^OB
^OB
^jB
^OB
_;B
_;B
`\B
`\B
`\B
`BB
`BB
`\B
`\B
`BB
_VB
_VB
_pB
`\B
`vB
`vB
`vB
`vB
`\B
`vB
abB
abB
bhB
b�B
cnB
cTB
dZB
d�B
dtB
dtB
dZB
dtB
d�B
d�B
c�B
c�B
dtB
e`B
ezB
ezB
dtB
d�B
d�B
d�B
e�B
f�B
ffB
ffB
ezB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
h�B
hsB
hsB
h�B
h�B
h�B
h�B
h�B
h�B
i�B
iyB
iyB
i�B
i�B
i�B
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<5��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711070035302017110700353020171107003530201806221232592018062212325920180622123259201804050428452018040504284520180405042845  JA  ARFMdecpA19c                                                                20171103093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171103003516  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171103003518  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171103003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171103003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171103003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171103003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171103003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171103003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171103003520                      G�O�G�O�G�O�                JA  ARUP                                                                        20171103005458                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171103153621  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20171106153530  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171106153530  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192845  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033259  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                