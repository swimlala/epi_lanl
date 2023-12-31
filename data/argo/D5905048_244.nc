CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-26T00:35:33Z creation;2018-05-26T00:35:37Z conversion to V3.1;2019-12-19T07:37:27Z update;     
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20180526003533  20200116231515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_244                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�evS?V 1   @�ew��� @4O������dM�	�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8fD8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� DcfDc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƃ3D��3D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�C3D��3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B{B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
B��
B��
B��
B��
B��
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
Bף�B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�ClCm�Co�CrCs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D8HD8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�DcHDcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��=D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qDƀ�D���D��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�z=D�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD逤D�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D� �D�@�D���D��q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��yA��mA��mA��`A��mA��yA��mA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��mA��`A�ĜA�oA�{A�AɓuA���A�33A�t�A�x�A�Q�A�"�A��AőhAľwA���AÓuA�VA£�A�^5A��A�jA�/A��A��A���A�9XA��A�ƨA�+A��RA�E�A�%A��;A�M�A��A���A��A�{A�l�A� �A��A�ffA���A��;A� �A�dZA��A���A���A�JA��FA�dZA��wA���A���A���A���A��A�{A���A�n�A� �A�ƨA�jA�z�A�A�ffA�;dA��\A���A�
=A�S�A���A�;dA��A��A���A�ZA�5?A�x�A��A���A�A�A���A�-A���A���A��RA�/A�/A�VA�A�A�"�A�Az��Ay�7Ax  Au��Ap$�Am�Ak�FAj�DAjr�Ah�\AfVAcS�A`=qA^r�A[��AY��AX1'AW�
AW��AVĜAS�wAR�RAR  AQp�AP��ANȴAL~�AJ~�AI��AH �AGl�AFQ�AE+AC��AAl�A@�9A@5?A?t�A=��A<(�A;t�A;�A:�yA:A�A8�DA7oA6  A4��A3��A1t�A/�A.r�A-7LA+�A+/A)O�A'A&�/A&=qA%?}A"�uA!K�A v�A�#AG�A��AƨA?}A�TA��A{A�-AK�A��At�AĜA^5A �A�AƨA�yAv�A �A�PA��A�HA{A�A"�A	�A�A\)A��AI�A��A
=A�mA�DA(�A�^A ��@��
@�ȴ@��@��D@�S�@�?}@��/@�Q�@���@���@�%@�K�@�7L@�+@��#@�j@�b@��
@���@���@�9@�1@�K�@�M�@���@�G�@��@��@�hs@�Ĝ@߶F@�;d@ް!@ݡ�@��/@܃@� �@�ƨ@�o@؋D@�I�@�bN@�;d@�p�@��@��@Ѓ@��@�J@Ͳ-@�G�@ˮ@ʧ�@���@�%@ǅ@�n�@ũ�@ēu@���@�n�@��^@�&�@�9X@�1@�;d@�V@�Ĝ@�1'@��F@�l�@���@�{@��@�9X@���@�V@��@�G�@��j@�Q�@�"�@���@�V@�5?@���@�`B@��/@��D@���@�;d@���@�E�@��@��@��-@���@�7L@�r�@�b@��w@�dZ@��H@�~�@�5?@��#@��@��@� �@�S�@�+@�"�@���@�^5@�-@���@���@�`B@�/@�z�@�(�@�1@��
@��@�33@�"�@�o@��y@���@��\@�M�@���@��7@�/@�j@�I�@��
@���@�\)@�+@�@��y@��@���@�ff@�@��-@�p�@�O�@���@��@�/@��@�%@���@��`@��@��@�\)@�;d@���@�~�@�E�@�$�@�@�G�@��@���@��j@��D@�9X@� �@��@���@���@�dZ@��@���@��+@�V@�5?@�@���@���@���@�p�@�?}@�&�@��/@�9X@��@��w@��@��;@�j@�Q�@�1@�l�@���@��+@�^5@�E�@�{@��#@���@���@��^@��h@�O�@���@���@���@�I�@��w@���@�t�@�C�@�"�@��@�@��y@��!@��@��#@���@���@�x�@�O�@�/@�%@���@���@���@�r�@�Z@�(�@��@�  @��;@���@�;d@�"�@��@�
=@���@���@��\@�ff@��@��@��#@���@���@��h@��h@��7@�x�@�G�@��/@��9@��D@�Q�@�1'@�b@���@��m@��
@��F@��P@�33@�
=@��!@�M�@�{@���@��^@���@���@���@��@�`B@�?}@�%@���@��`@��/@��9@��u@�Z@���@���@�S�@��@�
=@��y@���@�n�@��@�@��-@���@���@���@���@�@��^@���@�O�@��@+@\)@�@|�@+@+@�@~v�@}��@|�@{�m@{dZ@z��@z=q@z�@y�#@y��@y�7@yX@y7L@yG�@yG�@y&�@xbN@xb@w��@wl�@v��@vv�@v5?@v$�@v{@u�@u`B@t��@tI�@t�@s��@s�
@st�@sdZ@r�@rn�@q��@q�^@qx�@qX@q%@p�`@pĜ@p�u@p �@o�@o�w@o;d@nȴ@n$�@m`B@l�/@l�j@l�@lI�@kƨ@kC�@k@j��@j�!@j��@j-@i�^@ihs@i&�@hA�@g�;@g�@g��@f��@fV@e�h@d��@dj@d9X@d(�@ct�@b��@bn�@a�@a��@a�^@a��@ahs@aG�@a%@`Ĝ@`�u@`Q�@`  @_��@_;d@_
=@^�y@^��@^��@^v�@]�@]@]�@]?}@\�@\��@\�D@\Z@[��@[ƨ@[C�@Z�H@Z�\@ZJ@Y��@YG�@X��@XbN@XA�@X �@W�;@W;d@V�y@Vv�@V5?@V{@U�T@U��@UV@T�D@TI�@T1@S�F@S"�@R��@R��@Rn�@RJ@Q��@Q��@Q�7@Q7L@PQ�@O�w@O��@O|�@O�@N�@Nȴ@N��@Nff@N$�@M�@M@M�h@M/@MV@L�@Lz�@K��@Kt�@J�@J�H@J��@J��@J=q@J=q@I�#@I�^@I��@I�7@I%@H�u@H�@HA�@G�;@G�w@G;d@F��@F�R@F��@F��@F��@F�+@Fv�@FV@F$�@E�T@E��@E?}@D�/@D9X@Cƨ@C@B��@BM�@B=q@B-@A��@A��@AG�@@��@@r�@@A�@@b@@  @?�@?�@?��@?l�@?�@>�@>ff@>{@>@=�@=@=`B@<��@<��@<Z@<�@;�F@;t�@;S�@;o@:n�@9�#@9hs@9&�@9%@8��@81'@7�;@7��@7�w@7�P@7\)@6��@6�R@6�+@6{@5�T@5@5��@5�h@5O�@5V@4�j@49X@49X@3��@3��@3S�@2�H@2�!@2��@2�\@2M�@2J@1�^@1x�@1G�@0��@0��@0bN@0  @/��@/�w@/�P@/
=@.�y@.ȴ@.��@.E�@.$�@-�T@-�h@-/@-V@,�@,�/@,�/@,�j@,��@,j@,Z@,(�@+�F@+t�@+"�@*��@*~�@*�@)�@)��@)��@)�7@)7L@(��@(Ĝ@(�u@(bN@( �@(  @'�@'l�@'\)@'
=@&ff@&$�@%�@%�-@%�@%?}@$�/@$�@$��@$��@$��@$(�@#��@#ƨ@#��@#C�@#@"�@"��@"~�@"^5@"M�@"-@"�@!��@!��@!x�@!G�@!7L@ �`@ ��@ r�@ bN@ Q�@ Q�@ A�@ 1'@�@�@K�@
=@�y@�R@��@V@E�@{@�T@�h@`B@�@�/@�j@�@��@j@(�@��@ƨ@��@t�@dZ@o@��@M�@=q@=q@-@��@��@�7@x�@7L@�@%@��@��@�@�@bN@Q�@ �@  @�;@�@\)@
=@�y@�R@v�@V@5?@@�@�T@�@`B@O�@?}@?}@��@j@�@��@��@C�@"�@"�@o@@�H@��@�!@~�@-@J@��@�^@�^@hs@��@��@�@Q�@1'@b@�@�w@�P@l�@;d@
=@�y@�y@�@�@ȴ@��@�+@V@E�@{@��@�-@��@��@��@p�@/@V@�@�j@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��yA��mA��mA��`A��mA��yA��mA��yA��yA��A��A��A��A��A��A��A��A��A��A��A��A��mA��`A�ĜA�oA�{A�AɓuA���A�33A�t�A�x�A�Q�A�"�A��AőhAľwA���AÓuA�VA£�A�^5A��A�jA�/A��A��A���A�9XA��A�ƨA�+A��RA�E�A�%A��;A�M�A��A���A��A�{A�l�A� �A��A�ffA���A��;A� �A�dZA��A���A���A�JA��FA�dZA��wA���A���A���A���A��A�{A���A�n�A� �A�ƨA�jA�z�A�A�ffA�;dA��\A���A�
=A�S�A���A�;dA��A��A���A�ZA�5?A�x�A��A���A�A�A���A�-A���A���A��RA�/A�/A�VA�A�A�"�A�Az��Ay�7Ax  Au��Ap$�Am�Ak�FAj�DAjr�Ah�\AfVAcS�A`=qA^r�A[��AY��AX1'AW�
AW��AVĜAS�wAR�RAR  AQp�AP��ANȴAL~�AJ~�AI��AH �AGl�AFQ�AE+AC��AAl�A@�9A@5?A?t�A=��A<(�A;t�A;�A:�yA:A�A8�DA7oA6  A4��A3��A1t�A/�A.r�A-7LA+�A+/A)O�A'A&�/A&=qA%?}A"�uA!K�A v�A�#AG�A��AƨA?}A�TA��A{A�-AK�A��At�AĜA^5A �A�AƨA�yAv�A �A�PA��A�HA{A�A"�A	�A�A\)A��AI�A��A
=A�mA�DA(�A�^A ��@��
@�ȴ@��@��D@�S�@�?}@��/@�Q�@���@���@�%@�K�@�7L@�+@��#@�j@�b@��
@���@���@�9@�1@�K�@�M�@���@�G�@��@��@�hs@�Ĝ@߶F@�;d@ް!@ݡ�@��/@܃@� �@�ƨ@�o@؋D@�I�@�bN@�;d@�p�@��@��@Ѓ@��@�J@Ͳ-@�G�@ˮ@ʧ�@���@�%@ǅ@�n�@ũ�@ēu@���@�n�@��^@�&�@�9X@�1@�;d@�V@�Ĝ@�1'@��F@�l�@���@�{@��@�9X@���@�V@��@�G�@��j@�Q�@�"�@���@�V@�5?@���@�`B@��/@��D@���@�;d@���@�E�@��@��@��-@���@�7L@�r�@�b@��w@�dZ@��H@�~�@�5?@��#@��@��@� �@�S�@�+@�"�@���@�^5@�-@���@���@�`B@�/@�z�@�(�@�1@��
@��@�33@�"�@�o@��y@���@��\@�M�@���@��7@�/@�j@�I�@��
@���@�\)@�+@�@��y@��@���@�ff@�@��-@�p�@�O�@���@��@�/@��@�%@���@��`@��@��@�\)@�;d@���@�~�@�E�@�$�@�@�G�@��@���@��j@��D@�9X@� �@��@���@���@�dZ@��@���@��+@�V@�5?@�@���@���@���@�p�@�?}@�&�@��/@�9X@��@��w@��@��;@�j@�Q�@�1@�l�@���@��+@�^5@�E�@�{@��#@���@���@��^@��h@�O�@���@���@���@�I�@��w@���@�t�@�C�@�"�@��@�@��y@��!@��@��#@���@���@�x�@�O�@�/@�%@���@���@���@�r�@�Z@�(�@��@�  @��;@���@�;d@�"�@��@�
=@���@���@��\@�ff@��@��@��#@���@���@��h@��h@��7@�x�@�G�@��/@��9@��D@�Q�@�1'@�b@���@��m@��
@��F@��P@�33@�
=@��!@�M�@�{@���@��^@���@���@���@��@�`B@�?}@�%@���@��`@��/@��9@��u@�Z@���@���@�S�@��@�
=@��y@���@�n�@��@�@��-@���@���@���@���@�@��^@���@�O�@��@+@\)@�@|�@+@+@�@~v�@}��@|�@{�m@{dZ@z��@z=q@z�@y�#@y��@y�7@yX@y7L@yG�@yG�@y&�@xbN@xb@w��@wl�@v��@vv�@v5?@v$�@v{@u�@u`B@t��@tI�@t�@s��@s�
@st�@sdZ@r�@rn�@q��@q�^@qx�@qX@q%@p�`@pĜ@p�u@p �@o�@o�w@o;d@nȴ@n$�@m`B@l�/@l�j@l�@lI�@kƨ@kC�@k@j��@j�!@j��@j-@i�^@ihs@i&�@hA�@g�;@g�@g��@f��@fV@e�h@d��@dj@d9X@d(�@ct�@b��@bn�@a�@a��@a�^@a��@ahs@aG�@a%@`Ĝ@`�u@`Q�@`  @_��@_;d@_
=@^�y@^��@^��@^v�@]�@]@]�@]?}@\�@\��@\�D@\Z@[��@[ƨ@[C�@Z�H@Z�\@ZJ@Y��@YG�@X��@XbN@XA�@X �@W�;@W;d@V�y@Vv�@V5?@V{@U�T@U��@UV@T�D@TI�@T1@S�F@S"�@R��@R��@Rn�@RJ@Q��@Q��@Q�7@Q7L@PQ�@O�w@O��@O|�@O�@N�@Nȴ@N��@Nff@N$�@M�@M@M�h@M/@MV@L�@Lz�@K��@Kt�@J�@J�H@J��@J��@J=q@J=q@I�#@I�^@I��@I�7@I%@H�u@H�@HA�@G�;@G�w@G;d@F��@F�R@F��@F��@F��@F�+@Fv�@FV@F$�@E�T@E��@E?}@D�/@D9X@Cƨ@C@B��@BM�@B=q@B-@A��@A��@AG�@@��@@r�@@A�@@b@@  @?�@?�@?��@?l�@?�@>�@>ff@>{@>@=�@=@=`B@<��@<��@<Z@<�@;�F@;t�@;S�@;o@:n�@9�#@9hs@9&�@9%@8��@81'@7�;@7��@7�w@7�P@7\)@6��@6�R@6�+@6{@5�T@5@5��@5�h@5O�@5V@4�j@49X@49X@3��@3��@3S�@2�H@2�!@2��@2�\@2M�@2J@1�^@1x�@1G�@0��@0��@0bN@0  @/��@/�w@/�P@/
=@.�y@.ȴ@.��@.E�@.$�@-�T@-�h@-/@-V@,�@,�/@,�/@,�j@,��@,j@,Z@,(�@+�F@+t�@+"�@*��@*~�@*�@)�@)��@)��@)�7@)7L@(��@(Ĝ@(�u@(bN@( �@(  @'�@'l�@'\)@'
=@&ff@&$�@%�@%�-@%�@%?}@$�/@$�@$��@$��@$��@$(�@#��@#ƨ@#��@#C�@#@"�@"��@"~�@"^5@"M�@"-@"�@!��@!��@!x�@!G�@!7L@ �`@ ��@ r�@ bN@ Q�@ Q�@ A�@ 1'@�@�@K�@
=@�y@�R@��@V@E�@{@�T@�h@`B@�@�/@�j@�@��@j@(�@��@ƨ@��@t�@dZ@o@��@M�@=q@=q@-@��@��@�7@x�@7L@�@%@��@��@�@�@bN@Q�@ �@  @�;@�@\)@
=@�y@�R@v�@V@5?@@�@�T@�@`B@O�@?}@?}@��@j@�@��@��@C�@"�@"�@o@@�H@��@�!@~�@-@J@��@�^@�^@hs@��@��@�@Q�@1'@b@�@�w@�P@l�@;d@
=@�y@�y@�@�@ȴ@��@�+@V@E�@{@��@�-@��@��@��@p�@/@V@�@�j@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
(�B
(�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
(�B
)�B
0!B
/B
5?B
gmB
��B
�^B
ɺB
��B=qB� B�3B�qB��B�B�BoB<jB<jB9XB;dBT�B[#BgmBiyBgmBdZBp�Bv�Bw�Bp�BiyBgmBbNB_;BXBO�BP�B^5Bk�BgmBs�Bq�BhsBm�Bm�Bk�Bl�Bq�Bt�Bo�BiyBk�B�Bz�B]/B0!B'�B#�B��B�
B�BB
=B	7B
=B��B��BɺB��BbNB33B33B�B	7B�B�B�BB
��BB  BB
��B
�B
�B
��B
��B
�JB
_;B
L�B
2-B
7LB
!�B
DB	�B	��B	�B	��B	�RB	�=B	�VB	�%B	�B	�B	x�B	gmB	W
B	I�B	E�B	A�B	7LB	<jB	L�B	I�B	A�B	'�B	&�B	$�B	�B	�B	
=B��B��B��B��B��B�B�fB�HB��B�B�B��BǮBBǮBǮBƨB�qB�!B�B�B��B��B��B�oB�hB�\B�JB�JB�B~�B�B�By�BiyBt�B{�B~�B|�B|�Bv�Bs�BgmBiyBv�By�Bv�Bw�Bo�Bq�Bs�Br�BiyBffBiyBo�Bn�BiyB]/B^5B^5BXBR�BR�BQ�B]/BaHB]/B\)B^5BYBXBcTBdZB^5BdZBgmBdZBjBffBaHBk�BjBhsBdZBaHBcTBbNBcTBhsBl�Bo�Br�Bq�Bm�BgmBo�By�Bx�B|�B}�B{�Bx�Bt�Bx�B~�B�B�B�B�B�7B�=B�=B�+B}�B� B�=B�B}�B|�By�By�B|�B�B�7B�1B�B�+B�DB�DB�=B�PB�uB�uB��B��B��B��B��B��B��B��B��B�?B�RB�^B�^B�jB�wBÖBǮB�
B�#B�BB�`B�yB�mB�B�B��B��B��B	B	B	B	1B	PB	oB	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	'�B	)�B	-B	0!B	1'B	1'B	8RB	@�B	D�B	E�B	H�B	I�B	J�B	L�B	P�B	S�B	W
B	\)B	aHB	bNB	bNB	cTB	iyB	l�B	o�B	q�B	q�B	s�B	t�B	s�B	t�B	v�B	~�B	�B	�B	�B	�B	�+B	�7B	�7B	�1B	�1B	�7B	�=B	�PB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�9B	�9B	�3B	�3B	�?B	�FB	�^B	�jB	�qB	�wB	�}B	��B	��B	B	��B	��B	B	��B	��B	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�B	�#B	�#B	�)B	�BB	�HB	�HB	�TB	�ZB	�ZB	�ZB	�ZB	�TB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
%B
+B
%B
B
	7B

=B

=B
JB
JB
JB
DB
DB
VB
bB
oB
{B
{B
{B
{B
uB
oB
bB
JB
bB
{B
�B
�B
�B
�B
�B
�B
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
!�B
 �B
 �B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
%�B
&�B
&�B
&�B
(�B
)�B
+B
)�B
)�B
)�B
+B
,B
,B
,B
+B
+B
+B
+B
)�B
,B
-B
-B
+B
+B
+B
+B
-B
.B
.B
,B
,B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
/B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
2-B
33B
2-B
1'B
2-B
33B
33B
33B
33B
49B
49B
33B
49B
33B
33B
49B
33B
5?B
5?B
5?B
5?B
6FB
7LB
6FB
5?B
6FB
7LB
7LB
8RB
8RB
7LB
7LB
7LB
9XB
9XB
9XB
8RB
9XB
;dB
;dB
;dB
<jB
;dB
;dB
:^B
9XB
:^B
<jB
=qB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
=qB
=qB
>wB
=qB
<jB
<jB
=qB
>wB
?}B
?}B
?}B
?}B
@�B
?}B
@�B
@�B
@�B
?}B
@�B
B�B
A�B
A�B
B�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
C�B
C�B
C�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
D�B
E�B
E�B
F�B
E�B
E�B
D�B
D�B
F�B
G�B
G�B
H�B
H�B
H�B
G�B
F�B
F�B
G�B
G�B
H�B
I�B
I�B
I�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
I�B
H�B
I�B
J�B
K�B
L�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
L�B
M�B
M�B
M�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
O�B
Q�B
Q�B
Q�B
Q�B
P�B
R�B
T�B
S�B
S�B
S�B
S�B
T�B
T�B
VB
VB
VB
VB
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
XB
W
B
W
B
XB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
YB
ZB
ZB
[#B
[#B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
]/B
]/B
^5B
^5B
^5B
^5B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
`BB
_;B
`BB
aHB
aHB
aHB
aHB
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
dZB
cTB
dZB
cTB
e`B
e`B
e`B
e`B
e`B
dZB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
gmB
ffB
ffB
ffB
gmB
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
hsB
hsB
iyB
jB
k�B
jB
jB
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
l�B
m�B
m�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
n�B
o�B
p�B
o�B
p�B
q�B
q�B
q�B
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
q�B
q�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
u�B
u�B
v�B
w�B
w�B
w�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
)B
(�B
(
B
(�B
(�B
)B
)B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)B
*0B
0UB
/�B
6�B
iDB
�>B
�JB
ˬB
�B>�B�B�MB��B�dB�KB�wBB<�B=VB;0B=�BVB\)Bg�Bi�Bh
Be,Bq[BwfBxlBq�BjBhXBcB_�BYeBQhBR�B_Bl�Bh�Bt�BsMBj�BoiBo5Bm)BnBr�Bu�BqvBj�Bl�B�B|�B_�B4B)yB%,B�B�=B��B�BB
=B�B��B�BϫB��Bg�B7B5tBB�B�B1B�B�B
�dB�B�B3B
�rB
�]B
��B
��B
�BB
��B
d@B
O�B
4�B
8B
$@B
�B	��B	��B	�B	�(B	��B	��B	� B	��B	��B	��B	{B	j�B	Z�B	MjB	H1B	D�B	9�B	>B	M6B	JrB	C-B	+6B	(XB	%�B	�B	+B	�B��B�>B�VB��B��B�!B�>B�TB��B�	B��B�@B��BāBȀB�KB�_B��B�|B� B��B��B��B�EB�{B�[B�B�"B��B��B�B�SB�[B{�Bl�BvFB}B�B}�B}�BxBt�BiDBkBw�BzxBw�Bx�Bq[Br�BtTBshBkBg�Bj�BpUBoOBj�B_�B_�B_�BZ7BU2BT�BTB^Ba�B^5B]/B_pBZ�BY�BdBe,B_�BezBh>Be�BkBg�Bb�Bk�BkBiBeFBb�Bd�Bc�Bd�BiyBmCBp!Br�BrBncBh�Bp;Bz�By�B}qB~wB|�By�Bu�ByrB�B��B��B��B��B��B��B��B��B}B�OB�rB�%BHB~B{JB{B}�B��B��B��B�MB�B��B�B�^B�<B�,B�FB�7B��B�;B�NB�nB�DB��B��B��B��B��B��B�B�"B�HB�gBȴB�YBیB��B��B��B�>B��B��B�B�>B�dB	[B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	B	!B	!-B	#:B	$@B	(>B	*KB	-]B	0oB	1�B	1�B	8�B	@�B	D�B	FB	H�B	I�B	J�B	MB	Q4B	TFB	W�B	\xB	a|B	b�B	b�B	c�B	i�B	l�B	o�B	q�B	q�B	s�B	uB	tB	u%B	wLB	.B	�oB	�[B	�MB	�SB	�EB	�RB	�RB	��B	��B	��B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�]B	��B	�B	�:B	�2B	�8B	�RB	�>B	�]B	�]B	�;B	�UB	�oB	�nB	�nB	��B	��B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	ªB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�TB	уB	�pB	�B	�B	�B	�&B	�,B	�B	�B	�B	�9B	�9B	�YB	�QB	�qB	یB	ܒB	�\B	�bB	�|B	�B	�tB	�tB	�tB	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B	��B	�B	�B	�*B	�>B	�B	�6B	�"B	�(B	�(B	�(B	�B	�B	�.B	�(B	�<B	�.B	�HB
 OB
UB
-B
GB
3B
B
B
3B
3B
9B
SB
_B
EB
EB
tB
EB
tB
�B
	�B

�B

�B
dB
~B
~B
�B
�B
�B
bB
oB
{B
�B
�B
�B
�B
�B
�B
B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
 B
 �B
!�B
"�B
!�B
 �B
 �B
!�B
#B
#B
$B
$B
#�B
$&B
$B
$B
$�B
$�B
'B
&B
'B
'B
'B
'B
'B
(
B
&B
'B
'8B
'8B
)DB
*0B
+B
*0B
*KB
*KB
+6B
,"B
,"B
,"B
+6B
+6B
+6B
+6B
*eB
,"B
-)B
-)B
+QB
+QB
+kB
+QB
-CB
./B
./B
,WB
,qB
./B
.IB
/OB
0;B
0;B
0;B
0;B
0;B
/OB
0;B
0;B
0UB
0oB
1[B
2aB
2aB
2aB
33B
2GB
1vB
2GB
3MB
3hB
3hB
3MB
4nB
4TB
3hB
4TB
3�B
3�B
4TB
3hB
5tB
5�B
5tB
5�B
6`B
7fB
6zB
5�B
6`B
7�B
7�B
8lB
8lB
7�B
7�B
7�B
9�B
9rB
9�B
8�B
9�B
;B
;B
;B
<�B
;B
;B
:�B
9�B
:�B
<�B
=�B
<�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
=�B
=�B
>�B
=�B
<�B
<�B
=�B
>�B
?�B
?�B
?�B
?�B
@�B
?�B
@�B
@�B
@�B
?�B
@�B
B�B
A�B
A�B
B�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
C�B
C�B
C�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
D�B
E�B
E�B
F�B
E�B
E�B
D�B
D�B
F�B
G�B
G�B
H�B
H�B
H�B
G�B
F�B
F�B
G�B
G�B
H�B
I�B
I�B
I�B
H�B
IB
H�B
I�B
I�B
I�B
I�B
J�B
I�B
H�B
I�B
KB
K�B
MB
K�B
LB
MB
M�B
M�B
NB
NB
MB
M�B
NB
N"B
O�B
O�B
Q B
QB
Q B
QB
QB
PB
RB
RB
RB
R B
Q4B
SB
UB
T,B
T,B
T,B
TB
U2B
UB
V9B
V9B
VB
VB
VB
VB
V9B
U2B
V9B
W?B
W$B
W$B
X+B
W?B
W?B
XEB
YKB
ZQB
ZB
Z7B
Z7B
ZQB
Z7B
Z7B
ZQB
YeB
ZQB
Z7B
[WB
[=B
\CB
]IB
]IB
]dB
]IB
]IB
]dB
^OB
^OB
]dB
]dB
^OB
^jB
^OB
^jB
]~B
]dB
^OB
_pB
_pB
_VB
_pB
_VB
`\B
aHB
abB
`vB
_�B
`vB
abB
abB
abB
a|B
bhB
bhB
bhB
b�B
cnB
cnB
cnB
cnB
c�B
b�B
cnB
dZB
c�B
dtB
c�B
ezB
e`B
ezB
ezB
ezB
dtB
d�B
d�B
e�B
f�B
f�B
f�B
f�B
gmB
f�B
f�B
f�B
g�B
g�B
g�B
h�B
hsB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
h�B
h�B
i�B
jB
k�B
j�B
j�B
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
l�B
m�B
m�B
l�B
l�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
n�B
o�B
p�B
o�B
p�B
q�B
q�B
q�B
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
q�B
q�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
u�B
u�B
v�B
w�B
w�B
w�B
v�B
v�B
w�B
w�B
xB
x�B
x�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805300037532018053000375320180530003753201806221331052018062213310520180622133105201806042133352018060421333520180604213335  JA  ARFMdecpA19c                                                                20180526093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180526003533  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180526003536  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180526003536  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180526003537  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180526003537  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180526003537  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180526003537  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180526003537  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180526003537                      G�O�G�O�G�O�                JA  ARUP                                                                        20180526005623                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180526153644  CV  JULD            G�O�G�O�F�+�                JM  ARCAJMQC2.0                                                                 20180529153753  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180529153753  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604123335  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622043105  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231515                      G�O�G�O�G�O�                