CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-09-28T00:35:24Z creation;2017-09-28T00:35:27Z conversion to V3.1;2019-12-19T07:56:24Z update;     
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20170928003524  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_164                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�)u��Z 1   @�)v��-�@4��1'�d��2�W�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?�fD@fD@�fDA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du�fDvfDv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D��3D�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��
@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
B��
B��
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D�HD��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?�HD@HD@�HD@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Du�HDvHDvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�z=D��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD���D��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�:=D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�z=D�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD� �D�=qD�}qD��qD��qD�=qD�}qD���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A��hA��DA��A��A��A��A��A��A��A��A��A��A��A��+A��+A��+A�~�A�^5A��A�1AځA�r�A��HA��
A؉7A׺^A�5?A��A�{AГuA�I�A�7LA��A��`A�"�AƸRAŅA�jAÑhA\A��+A�/A�bNA��mA��A��HA�K�A�dZA�VA��mA��hA�I�A�+A�VA��A�(�A���A�dZA�$�A��A�~�A���A�ƨA��7A�^5A�/A��A�(�A�ZA� �A���A�z�A��A�%A�hsA��A�?}A�l�A��A� �A���A���A�&�A��\A�;dA��#A��A��7A��DA�?}A���A�{A�9XA�^5A�VA���A�ffA�$�A��A�33A��;A�&�A�A�
=A��/A��A�ȴA�Q�A��uA�bA�bA���A���A��uA��A�G�A��/A�9XA��/A��RA���A~bA}hsA|bA{��A{�AxjAv  At�!As��As�FAr�DAo�mAl�AjĜAe�hAa��A`�uA\��AZ��AY��AYC�AX�+AW��AW/AV��AT��AS��ARn�AQ�TAP{AN��AM��AL��AK��AJ��AJJAIG�AG�AD�AB��AAp�A@(�A>bNA=&�A;+A9�7A8A�A7��A6��A4�A3C�A2�A0�jA/��A-�7A,�DA,�A+�hA*ĜA)�A'XA&  A%�FA%�7A%hsA%K�A$�jA#ƨA"�A!K�A��A��A^5A�`AA�A�AXA�`AffAƨA�HA�^AAVAA�A�#AG�A9XAG�A�`A�AQ�AK�A
�uA	��A	?}A�9A��A�A�A(�A|�A{At�A�A �yA �@��
@��H@���@�5?@��D@�bN@��D@�&�@�@�&�@�^5@�J@�?}@�-@��@���@��@���@� �@�1@�7L@�^@�D@�l�@��@�(�@߅@��@�bN@�C�@��@���@ݙ�@�hs@��@ܴ9@�l�@��@��
@���@���@��@�A�@���@���@�/@�r�@��@ɑh@���@�9X@Ǯ@�M�@��@Ĵ9@öF@�5?@�/@��u@�o@���@�@��`@���@���@��T@�@�hs@�`B@��/@�Q�@���@���@�K�@��@��@��T@��h@��@�I�@� �@��@��@�33@�o@���@���@��\@�ff@�=q@�{@�J@��@���@���@�X@��@���@�|�@�S�@��@��H@��+@�5?@�J@��^@�?}@���@�bN@�ƨ@�K�@�;d@��@�@���@��#@���@�p�@�G�@�/@�Ĝ@��@�ƨ@�;d@���@��R@�E�@��@��@��T@��^@���@��7@�hs@�%@���@��9@���@�z�@�r�@�I�@� �@��@�ƨ@��F@��F@�|�@�;d@�+@��@�
=@�n�@�x�@��@�A�@�(�@��@�  @��@��;@��w@���@�K�@�@��\@�5?@��@��#@���@�X@�7L@��@���@��@�z�@�I�@�1'@�b@��@�ƨ@���@��@�S�@�;d@��y@�v�@�$�@���@��-@���@�7L@�%@��9@���@��D@�j@�I�@��@�b@�  @��m@��w@�;d@�o@���@��R@���@��\@�n�@�V@�E�@�-@���@��^@���@��@�G�@��`@��u@��@���@��
@�dZ@��@��@���@���@�v�@�E�@�$�@�@�J@�@�J@���@���@��@�X@�G�@�G�@�G�@�O�@�G�@�G�@�G�@�G�@��@�&�@��@���@��`@�bN@��@�K�@��!@�v�@�V@�V@�-@���@��-@�V@��D@�(�@�  @��@���@��@�33@�n�@�$�@���@���@���@��@�O�@�/@�/@�&�@�&�@��@��/@�j@�9X@��@�  @�ƨ@�l�@��@��\@�ff@�V@�E�@�E�@�E�@�5?@���@��-@�hs@��@��@�V@���@��D@�bN@�I�@� �@��m@��F@��@�l�@�dZ@�;d@��@��@���@�v�@�n�@�=q@���@���@��7@�`B@�/@�V@��`@��u@�j@� �@��@�@�w@�w@�@l�@~��@~v�@~E�@}p�@|z�@{�m@{�F@{t�@{S�@{33@{@z��@zn�@yx�@xb@w\)@w�@v�y@v�@v�R@v��@vv�@vE�@v$�@u��@u`B@tj@sS�@r�H@r�H@r�@q�7@qX@p�`@pr�@pb@o�w@o|�@o+@nȴ@n��@n{@m��@mO�@l�/@l�D@l1@kt�@kC�@ko@k@j�H@j��@j-@i��@iX@hĜ@h�@hQ�@h �@g�@f��@f�R@f�R@f��@f�+@fV@e@d��@dj@d�@c��@c�m@cdZ@b��@b=q@b�@bJ@a�#@a��@a��@a%@`��@`��@`A�@`b@`  @`  @_�;@_�@_�@_|�@^�@^5?@]@]�@\��@\Z@[�F@[t�@[C�@["�@[o@Z~�@Y�@Y��@Y�7@X��@X��@X��@XA�@W�@V��@V��@V$�@U�@U?}@T�@T�D@S�m@St�@R�H@R��@R�\@R^5@R-@R-@Q��@Q&�@Q�@PĜ@P�u@P�@PA�@P �@P  @O�;@O��@O��@O�@Nff@Nff@NE�@M�-@M�@L��@L�j@L�D@LZ@L(�@L1@K�F@Kt�@Ko@J�!@J~�@Jn�@JM�@J-@I��@H�9@Hr�@Hb@G�;@Gl�@G;d@G�@F��@FV@F$�@E�-@E�h@E`B@EV@Dj@Ct�@C33@B��@BJ@A�^@Ahs@A%@@�9@@r�@@ �@?�;@?�w@?��@?\)@?�@>�@>��@>v�@>E�@>$�@>@=@=/@<��@<Z@<1@;�F@;t�@;33@:�@:~�@:M�@:=q@9��@97L@8�9@8r�@8A�@81'@8A�@8 �@7�;@7�;@7��@7�@7��@7|�@7�@7
=@6�@6�R@65?@5�@5�-@5p�@5?}@5�@5�@5V@4�@4��@4I�@3��@3ƨ@3dZ@3"�@2�!@1��@1��@1��@1��@1hs@1X@1�@0�9@0r�@0bN@0b@/�@/�P@/|�@.��@.�+@.ff@-@-V@,�D@,z�@,j@,Z@,Z@,(�@,1@+ƨ@+�@+"�@*��@*=q@*J@)�@)�^@)G�@(��@(Ĝ@(1'@'��@'��@'l�@';d@'
=@&��@&�y@&ȴ@&�+@&�+@&�+@&�+@&��@&��@&��@&��@&�+@&�+@&V@&5?@%�@%��@%p�@%`B@%/@%V@$��@$�@$�D@$�D@$Z@$�@#�@#dZ@#dZ@#33@#@"�@"�H@"��@"��@"�!@"~�@"�@!��@!�@!�^@!x�@!7L@!�@ �9@ �@ A�@ 1'@ 1'@  �@   @��@�w@�@�@��@|�@\)@;d@+@
=@��@��@�y@�@�+@V@$�@�T@�@�@�D@j@j@j@j@j@j@Z@Z@Z@I�@I�@9X@1@�@S�@33@o@�@��@�\@^5@M�@�@�#@��@��@�^@��@hs@X@G�@7L@%@��@��@Q�@b@�@�;@�;@�w@�P@;d@��@�y@ȴ@�R@�+@ff@{@��@�@`B@/@V@�/@�j@j@�@��@�F@�@dZ@S�@S�@S�@C�@C�@33@"�@@��@^5@-@J@�#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A��hA��DA��A��A��A��A��A��A��A��A��A��A��A��+A��+A��+A�~�A�^5A��A�1AځA�r�A��HA��
A؉7A׺^A�5?A��A�{AГuA�I�A�7LA��A��`A�"�AƸRAŅA�jAÑhA\A��+A�/A�bNA��mA��A��HA�K�A�dZA�VA��mA��hA�I�A�+A�VA��A�(�A���A�dZA�$�A��A�~�A���A�ƨA��7A�^5A�/A��A�(�A�ZA� �A���A�z�A��A�%A�hsA��A�?}A�l�A��A� �A���A���A�&�A��\A�;dA��#A��A��7A��DA�?}A���A�{A�9XA�^5A�VA���A�ffA�$�A��A�33A��;A�&�A�A�
=A��/A��A�ȴA�Q�A��uA�bA�bA���A���A��uA��A�G�A��/A�9XA��/A��RA���A~bA}hsA|bA{��A{�AxjAv  At�!As��As�FAr�DAo�mAl�AjĜAe�hAa��A`�uA\��AZ��AY��AYC�AX�+AW��AW/AV��AT��AS��ARn�AQ�TAP{AN��AM��AL��AK��AJ��AJJAIG�AG�AD�AB��AAp�A@(�A>bNA=&�A;+A9�7A8A�A7��A6��A4�A3C�A2�A0�jA/��A-�7A,�DA,�A+�hA*ĜA)�A'XA&  A%�FA%�7A%hsA%K�A$�jA#ƨA"�A!K�A��A��A^5A�`AA�A�AXA�`AffAƨA�HA�^AAVAA�A�#AG�A9XAG�A�`A�AQ�AK�A
�uA	��A	?}A�9A��A�A�A(�A|�A{At�A�A �yA �@��
@��H@���@�5?@��D@�bN@��D@�&�@�@�&�@�^5@�J@�?}@�-@��@���@��@���@� �@�1@�7L@�^@�D@�l�@��@�(�@߅@��@�bN@�C�@��@���@ݙ�@�hs@��@ܴ9@�l�@��@��
@���@���@��@�A�@���@���@�/@�r�@��@ɑh@���@�9X@Ǯ@�M�@��@Ĵ9@öF@�5?@�/@��u@�o@���@�@��`@���@���@��T@�@�hs@�`B@��/@�Q�@���@���@�K�@��@��@��T@��h@��@�I�@� �@��@��@�33@�o@���@���@��\@�ff@�=q@�{@�J@��@���@���@�X@��@���@�|�@�S�@��@��H@��+@�5?@�J@��^@�?}@���@�bN@�ƨ@�K�@�;d@��@�@���@��#@���@�p�@�G�@�/@�Ĝ@��@�ƨ@�;d@���@��R@�E�@��@��@��T@��^@���@��7@�hs@�%@���@��9@���@�z�@�r�@�I�@� �@��@�ƨ@��F@��F@�|�@�;d@�+@��@�
=@�n�@�x�@��@�A�@�(�@��@�  @��@��;@��w@���@�K�@�@��\@�5?@��@��#@���@�X@�7L@��@���@��@�z�@�I�@�1'@�b@��@�ƨ@���@��@�S�@�;d@��y@�v�@�$�@���@��-@���@�7L@�%@��9@���@��D@�j@�I�@��@�b@�  @��m@��w@�;d@�o@���@��R@���@��\@�n�@�V@�E�@�-@���@��^@���@��@�G�@��`@��u@��@���@��
@�dZ@��@��@���@���@�v�@�E�@�$�@�@�J@�@�J@���@���@��@�X@�G�@�G�@�G�@�O�@�G�@�G�@�G�@�G�@��@�&�@��@���@��`@�bN@��@�K�@��!@�v�@�V@�V@�-@���@��-@�V@��D@�(�@�  @��@���@��@�33@�n�@�$�@���@���@���@��@�O�@�/@�/@�&�@�&�@��@��/@�j@�9X@��@�  @�ƨ@�l�@��@��\@�ff@�V@�E�@�E�@�E�@�5?@���@��-@�hs@��@��@�V@���@��D@�bN@�I�@� �@��m@��F@��@�l�@�dZ@�;d@��@��@���@�v�@�n�@�=q@���@���@��7@�`B@�/@�V@��`@��u@�j@� �@��@�@�w@�w@�@l�@~��@~v�@~E�@}p�@|z�@{�m@{�F@{t�@{S�@{33@{@z��@zn�@yx�@xb@w\)@w�@v�y@v�@v�R@v��@vv�@vE�@v$�@u��@u`B@tj@sS�@r�H@r�H@r�@q�7@qX@p�`@pr�@pb@o�w@o|�@o+@nȴ@n��@n{@m��@mO�@l�/@l�D@l1@kt�@kC�@ko@k@j�H@j��@j-@i��@iX@hĜ@h�@hQ�@h �@g�@f��@f�R@f�R@f��@f�+@fV@e@d��@dj@d�@c��@c�m@cdZ@b��@b=q@b�@bJ@a�#@a��@a��@a%@`��@`��@`A�@`b@`  @`  @_�;@_�@_�@_|�@^�@^5?@]@]�@\��@\Z@[�F@[t�@[C�@["�@[o@Z~�@Y�@Y��@Y�7@X��@X��@X��@XA�@W�@V��@V��@V$�@U�@U?}@T�@T�D@S�m@St�@R�H@R��@R�\@R^5@R-@R-@Q��@Q&�@Q�@PĜ@P�u@P�@PA�@P �@P  @O�;@O��@O��@O�@Nff@Nff@NE�@M�-@M�@L��@L�j@L�D@LZ@L(�@L1@K�F@Kt�@Ko@J�!@J~�@Jn�@JM�@J-@I��@H�9@Hr�@Hb@G�;@Gl�@G;d@G�@F��@FV@F$�@E�-@E�h@E`B@EV@Dj@Ct�@C33@B��@BJ@A�^@Ahs@A%@@�9@@r�@@ �@?�;@?�w@?��@?\)@?�@>�@>��@>v�@>E�@>$�@>@=@=/@<��@<Z@<1@;�F@;t�@;33@:�@:~�@:M�@:=q@9��@97L@8�9@8r�@8A�@81'@8A�@8 �@7�;@7�;@7��@7�@7��@7|�@7�@7
=@6�@6�R@65?@5�@5�-@5p�@5?}@5�@5�@5V@4�@4��@4I�@3��@3ƨ@3dZ@3"�@2�!@1��@1��@1��@1��@1hs@1X@1�@0�9@0r�@0bN@0b@/�@/�P@/|�@.��@.�+@.ff@-@-V@,�D@,z�@,j@,Z@,Z@,(�@,1@+ƨ@+�@+"�@*��@*=q@*J@)�@)�^@)G�@(��@(Ĝ@(1'@'��@'��@'l�@';d@'
=@&��@&�y@&ȴ@&�+@&�+@&�+@&�+@&��@&��@&��@&��@&�+@&�+@&V@&5?@%�@%��@%p�@%`B@%/@%V@$��@$�@$�D@$�D@$Z@$�@#�@#dZ@#dZ@#33@#@"�@"�H@"��@"��@"�!@"~�@"�@!��@!�@!�^@!x�@!7L@!�@ �9@ �@ A�@ 1'@ 1'@  �@   @��@�w@�@�@��@|�@\)@;d@+@
=@��@��@�y@�@�+@V@$�@�T@�@�@�D@j@j@j@j@j@j@Z@Z@Z@I�@I�@9X@1@�@S�@33@o@�@��@�\@^5@M�@�@�#@��@��@�^@��@hs@X@G�@7L@%@��@��@Q�@b@�@�;@�;@�w@�P@;d@��@�y@ȴ@�R@�+@ff@{@��@�@`B@/@V@�/@�j@j@�@��@�F@�@dZ@S�@S�@S�@C�@C�@33@"�@@��@^5@-@J@�#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B`BB`BB`BBaHB`BB`BB`BB`BB`BB`BB`BB_;B_;B_;B^5B]/B[#BW
BP�BC�B5?BA�B~�Bw�Bl�Bt�Bl�BffBm�BgmB�bB��B�9B�XB�}B�dB��B��BBbB�B �B.B)�B"�B1'B �B!�B�B,B,B"�BuBVB(�B%�B.B<jBL�BYB^5B`BBhsBn�Bo�Br�Bp�Bm�Bk�Bm�BiyBp�Bo�B[#BR�BYBYBL�BI�BK�B@�B@�B0!B+B$�B&�B �BhB%B1BuB\BBB��B�B�;B��B�}B��B�=B{�Bl�BW
BC�BD�B49BDB
�B
�yB
�`B
��B
ĜB
�'B
�dB
�'B
��B
�VB
q�B
u�B
o�B
O�B
2-B
L�B
H�B
I�B
C�B
,B
�B
�B
�B
�B
DB	�B	��B	�wB	��B	|�B	�B	hsB	cTB	gmB	jB	e`B	cTB	bNB	dZB	VB	ZB	T�B	Q�B	C�B	7LB	33B	(�B	�B	�B	VB	1B��B�/B�#B�BB�BɺB��B�9B��B��B��B��B�By�B�Bx�B{�Bo�Bu�Bz�Bx�Bo�BiyB`BBe`Bu�Bu�Bt�Br�BjBbNB\)BffBgmBl�Bq�Bl�Bq�Bs�Bo�BbNB|�B{�Bu�Br�Bm�Bp�Bs�Bq�Bm�Be`BgmBm�Be`B[#B^5Be`BjBo�Bs�Bt�BdZB[#Be`Bk�BffBp�Bt�Bu�Bs�Bl�Bs�Bu�By�Bw�B~�B�+B�DB��B��B��B��B��B�uB�\B�B�B�bB��B��B�oB�=B�bB�uB�oB�uB��B�B�B�B�XB�^B�}B�}B�qB�jB�FB�B�B��B�B��B�B�B�3B�9B�9B�B�XB�wB�jB�}B�wB�qBB�qB�dB��B��BȴB��B��B��B�B��B�B�NB�NB�ZB�sB�B�B��B��B��B��B��B	B	+B	+B	PB	\B	hB	hB	�B	�B	�B	�B	 �B	#�B	'�B	)�B	)�B	(�B	+B	)�B	.B	,B	5?B	8RB	8RB	8RB	8RB	9XB	;dB	;dB	;dB	=qB	=qB	B�B	H�B	L�B	L�B	L�B	K�B	J�B	R�B	VB	W
B	XB	VB	XB	_;B	aHB	ffB	iyB	jB	n�B	r�B	r�B	r�B	s�B	t�B	t�B	t�B	x�B	z�B	{�B	{�B	|�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�1B	�1B	�+B	�B	�+B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�3B	�RB	�dB	�^B	�dB	�jB	�jB	�wB	�}B	��B	��B	B	B	B	ŢB	ǮB	ȴB	ɺB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�;B	�BB	�;B	�NB	�TB	�ZB	�mB	�mB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
%B
%B
%B
	7B
1B
1B
1B
	7B

=B

=B
	7B

=B
DB
JB
JB
JB
JB
JB
JB
VB
\B
VB
PB
\B
\B
bB
bB
hB
hB
bB
oB
oB
uB
�B
�B
�B
{B
{B
uB
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
!�B
!�B
!�B
"�B
"�B
$�B
%�B
%�B
&�B
'�B
(�B
)�B
+B
)�B
)�B
(�B
'�B
,B
,B
-B
-B
+B
)�B
,B
.B
.B
.B
.B
.B
-B
/B
/B
/B
0!B
1'B
1'B
1'B
2-B
2-B
1'B
0!B
0!B
2-B
2-B
2-B
33B
33B
5?B
5?B
5?B
5?B
49B
49B
5?B
6FB
5?B
6FB
6FB
5?B
5?B
5?B
6FB
7LB
6FB
8RB
8RB
8RB
7LB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
9XB
9XB
;dB
;dB
;dB
<jB
;dB
<jB
<jB
<jB
<jB
;dB
;dB
;dB
=qB
=qB
;dB
<jB
>wB
>wB
>wB
>wB
?}B
?}B
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
>wB
>wB
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
D�B
D�B
C�B
F�B
E�B
F�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
K�B
L�B
M�B
M�B
N�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
N�B
O�B
P�B
Q�B
R�B
R�B
Q�B
Q�B
R�B
R�B
R�B
R�B
Q�B
Q�B
R�B
R�B
R�B
Q�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
W
B
XB
XB
XB
XB
W
B
W
B
XB
YB
XB
XB
YB
YB
XB
YB
ZB
YB
YB
[#B
\)B
\)B
]/B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
bNB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
bNB
bNB
bNB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
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
gmB
gmB
hsB
gmB
ffB
hsB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
jB
jB
jB
iyB
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
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
p�B
p�B
q�B
q�B
q�B
p�B
q�B
r�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B`\B`\B`\BaHB`BB`BB`BB`BB`\B`\B`BB_VB_VB_;B^5B]/B[=BWYBQ�BE�B:�BE�B.By>Bn�Bu�Bn�Bi�Bp�Bm)B�@B�zB��B�6B��B�;B�BB�*BmBoB�B"�B/5B,B%�B3B#:B#:B �B,�B,�B$&BmB B+B(sB/�B=VBM�BY�B^�BaHBi_BoBp!Bs3BqABn}Bm]Bo�Bk�Bq�Bp�B^BUMBZkBZBN�BK�BL�BBABA�B2GB,qB&2B'�B"4B�B	B
#BFBbB?BB�0B��B��B��BB��B�jB~�Bo B[WBE�BE�B6�B}B
�TB
�QB
��B
ՁB
�EB
��B
�B
��B
�4B
��B
u%B
w2B
q�G�O�B
5�B
M�B
J=B
JrB
EB
/5B
"hB
!B
�B
kB
6B	�B	��B	��B	�pB	�oB	�GB	l�B	e�B	h�B	k6B	ffB	dtB	c B	eFB	X�B	[qB	V�B	R�B	E�B	8�B	4�B	*KB	VB	�B	�B		�B�B�HB�~B��B��B��BB��B��B�mB��B��B��B|B��Bz�B}qBr|Bv�B{�By�BqBkQBcBf�Bv+Bv+Bu%BsBk�Bc�B^OBg�Bi�Bm�Br�BncBr�BtnBqBeFB}�B|�Bw2Bt�Bo�Bq�Bt�BraBn�Bf�Bh�BncBf�B]dB_�BffBk�Bp�Bt�BvG�O�B]~Bf�Bl�Bh>Bq�BuZBv+Bt9Bm�BtTBv+Bz^Bx�BB�B�)B��B��B�QB�XB��B��B�B�+B��B�B��B�dB�{B�~B�hB�{B�uB��B��B�wB��B��B��B�B��B��B��B��B�LB�iB��B��B�B�kB�WB�5B��B��B�B��B��B��B�"B�4B�cB�BB�-B�BB��B�UB�^BɠB�oBөB��B��BԯBؓB�hB�B�B��B��B�B��B�DB�PB�.B��B	aB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	$B	($B	*B	*B	)DB	+6B	*eB	.cB	-B	5tB	8�B	8�B	8�B	8�B	9�B	;�B	;�B	;�B	=�B	>B	B�B	IB	L�B	MB	MB	LB	K^B	S&B	V9B	WYB	X_B	VmB	X�B	_�B	a�B	f�B	i�B	j�B	n�B	r�B	r�B	r�B	s�B	t�B	uB	u%B	y	B	z�B	|B	|B	}B	}"B	~BB	�4B	�GB	�MB	�9B	�MB	�SB	�KB	�KB	�_B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�:B	�FB	�DB	�WB	�5B	�cB	�OB	�GB	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	� B	�B	�&B	�&B	� B	�:B	�HB	�,B	�MB	�9B	�$B	�$B	�$B	�EB	�EB	�+B	�EB	�_B	�7B	�QB	�KB	�B	�kB	�xB	�VB	��B	ߊB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	��B	�B	�B	��B	�B	�B	��B	��B	��B	��B	�-B	�'B	�!B	� B	��B	��B	��B	�B	��B	��B	�'B	�B	�B	�B	��B	�B	�B	�JB	�xB	�BB	�.B	�B	�B	�B
 B
 B
'B
'B
'B
'B
 iB
 OB
[B
3B
GB
aB
uB
oB
AB
3B
SB
SB
%B
9B
SB
�B
?B
YB
tB
	7B
KB
�B
fB
	lB

XB

rB
	�B

rB
xB
~B
~B
dB
dB
�B
�B
�B
vB
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
 B
!B
!�B
!�B
"�B
"�B
!�B
"B
!�B
#B
#B
%B
%�B
%�B
'8B
(>B
)*B
)�B
+B
*B
*B
)DB
(XB
,"B
,"B
-)B
-CB
+QB
*KB
,"B
./B
.IB
./B
./B
.IB
-CB
/5B
/5B
/5B
0;B
1'B
1AB
1AB
2GB
2-B
1AB
0oB
0UB
2aB
2aB
2aB
3hB
3�B
5tB
5tB
5tB
5tB
4�B
4�B
5ZB
6`B
5tB
6zB
6zB
5tB
5�B
5�B
6�B
7�B
6zB
8lB
8�B
8�B
7�B
8�B
8�B
9�B
:^B
:xB
:xB
:�B
9�B
9�B
;�B
;B
;B
<�B
;B
<�B
<�B
<�B
<�B
;B
;�B
;�B
=qB
=�B
;�B
<�B
>�B
>�B
>�B
>�B
?�B
?�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
>�B
>�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
D�B
D�B
C�B
F�B
E�B
F�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
MB
MB
MB
MB
L�B
L�B
K�B
MB
N"B
M�B
N�B
OB
OB
O�B
OB
O�B
O�B
P.B
OB
PB
Q B
RB
R�B
SB
RB
RB
SB
R�B
SB
R�B
RB
RB
SB
S&B
S&B
R B
SB
T,B
TB
TB
UB
T�B
UB
UB
TB
T,B
TB
UB
U2B
U2B
UMB
UMB
W$B
XB
X+B
XEB
XEB
W?B
W?B
X+B
Y1B
XEB
X+B
Y1B
YKB
X_B
YKB
ZQB
YeB
YeB
[=B
\CB
\)B
]IB
\)B
\CB
\CB
\]B
\CB
\]B
\]B
]IB
^OB
^OB
^OB
^jB
^jB
_VB
^jB
_VB
`\B
`\B
`\B
abB
aHB
abB
abB
a|B
bNB
bNB
bhB
bhB
bhB
bhB
bNB
aHB
bhB
a|B
a|B
abB
abB
bhB
bhB
bhB
bhB
bhB
cnB
cnB
cnB
b�B
b�B
b�B
dtB
dtB
d�B
d�B
ezB
e`B
ezB
e`B
dtB
d�B
d�B
e�B
ezB
e�B
e�B
e�B
ezB
e�B
f�B
f�B
g�B
gmB
gmB
g�B
g�B
gmB
hsB
gmB
gmB
g�B
g�B
hsB
h�B
h�B
hsB
hsB
hsB
h�B
g�B
g�B
h�B
g�B
f�B
h�B
jB
j�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
j�B
j�B
j�B
i�B
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
o�B
o�B
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
p�B
p�B
q�B
q�B
q�B
p�B
q�B
r�B
q�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
r�B
s�B
s�B
t�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.08(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710020035112017100200351120171002003511201806221319482018062213194820180622131948201804050722212018040507222120180405072221  JA  ARFMdecpA19c                                                                20170928093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170928003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170928003526  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170928003526  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170928003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170928003527  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170928003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170928003527  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170928003527  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170928003527                      G�O�G�O�G�O�                JA  ARUP                                                                        20170928005614                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170928153906  CV  JULD            G�O�G�O�F�K�                JM  ARSQJMQC2.0                                                                 20170929000000  CF  PSAL_ADJUSTED_QCCv  C�  G�O�                JM  ARCAJMQC2.0                                                                 20171001153511  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171001153511  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222221  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041948  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                