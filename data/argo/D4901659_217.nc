CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-07-08T11:00:26Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݌   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8        �8Argo profile    3.1 1.2 19500101000000  20200708110026  20230721230927  4901659 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5382                            2C  D   NAVIS_A                         0371                            082713                          863 @�'�I�C1   @�'\�2@<f$�/��c��G�{1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      �A   A   A   @�ff@�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ D�|�D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��
@�p�A Q�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�BG�BG�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
B���B��
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
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�z>DֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�>D��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD�q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AӉ7Aә�Aӝ�Aӝ�Aӝ�Aӡ�Aӣ�Aӧ�Aӧ�Aӣ�Aӟ�Aӝ�Aӛ�AӓuA�hsA�I�A���A�E�A�^5A�Q�A�(�A�l�A���A�z�A���Aç�A�1A�I�A���A���A��-A�XA���A�t�A�A�XA���A�O�A��A�"�A��!A�oA�jA��#A��A��mA���A�1A�$�A�A�A�z�A��yA�S�A��^A�"�A��hA���A�bNA��A��A���A�jA�{A�ffA�%A�hsA��^A�=qA�  A���A�"�A�oA��A���A�A��9A�v�A�O�A�bNA�O�A�(�A�"�A�p�A�l�A��hA��FA�\)A��A���A��A��uA��A�{A���A�XA�hsA���A��A�hsA�JA��RA�1A�1A���A�v�A�A��9A��hA�hsA�1'A�JA�wAA}��A{��Av��Asp�ArE�Ao�Am�AmK�Al�9Ak��Ai�Ag%AfbAet�AeVAd-Ac�^Ac\)Ab��Abv�AaG�A_O�A]A[S�AY?}AW��AV{AUx�AT��ASO�AQ�#AP��AP�AO\)AM\)AL-AL  AK��AK%AJ1AGdZAE��ADZAC��AC7LAC%AB�`ABĜABZAA�
A@�9A?K�A>�jA=S�A;�wA;dZA9�;A8E�A6��A5��A57LA4A�A3�PA3%A2jA1�A1oA0M�A/oA-��A-|�A-33A,1'A+%A*z�A)�^A(��A'��A&��A&��A&�DA&  A%x�A%�A#t�A"Q�A!G�A ��A ��A (�A�PA�DAx�A�yA��AȴAZA�A?}A�AO�AVA/A�mAS�A��AbNA�
A7LA�DAO�A��A��A;dA	�TA	VA��AM�A��A��AhsA��A1A��A��A�!A�TA�PAAffA5?AJA�
A��AXA 1'@�M�@���@�K�@���@��`@�K�@�V@��@��@�`B@�%@�9X@�dZ@�n�@��#@��@��@�;d@��@�$�@��#@�@�V@�^5@�G�@㕁@�33@��@���@��m@��@ݺ^@��`@�+@ץ�@�hs@�z�@�1@��@Η�@���@�E�@Ɂ@ǍP@�V@��@�I�@å�@���@��H@�{@�hs@��@���@�33@�^5@�p�@��`@��@��w@�33@��@���@�J@��-@��@�%@��u@�r�@�1'@�@�@�b@�+@�$�@��;@�ff@��h@�j@�+@�V@�O�@���@��@��m@�
=@�M�@�E�@�$�@��T@��@��@��;@�S�@���@��R@�^5@�@�hs@��@��`@��9@�r�@���@��@��\@�V@�@�?}@���@�Ĝ@��u@��;@�|�@���@�7L@�&�@�V@��9@� �@���@��w@�C�@��y@���@��@��h@��@�hs@�`B@�&�@�z�@���@�ƨ@���@��@�|�@�\)@�"�@�@��@��@��R@�v�@�J@��h@�`B@���@���@�;d@���@�n�@�$�@��#@���@��@�/@��@��u@�r�@�  @���@��@�l�@�dZ@�\)@�S�@�K�@�C�@�"�@��y@�ff@��#@���@��@�`B@�?}@��@��@� �@�ƨ@�@���@��\@�~�@�v�@�M�@�{@�@��@���@�@��-@���@��h@�p�@�?}@�V@���@��`@���@�  @���@��w@��w@���@�C�@���@��H@��H@���@��!@���@�v�@��@��@���@��@�/@�V@���@��@�z�@�z�@�Z@�  @�@�P@K�@�@~E�@}�T@}�@|�@|1@{ƨ@{dZ@{o@z�!@z�@y��@y��@y�7@x��@x�u@xb@w\)@w;d@v��@v�@vȴ@vE�@u�@u?}@t��@s��@sC�@r��@rM�@rJ@q�#@q��@q��@q��@qhs@q%@pĜ@p�u@pbN@pA�@pb@o�w@o�P@o\)@n�@n�R@nff@n5?@n5?@n@n@m�T@m�-@m�-@m/@l�@l��@l��@l9X@k�@j�@j��@jM�@i�#@ix�@h��@h �@g�@g�w@gl�@g+@g
=@f��@fV@f$�@e��@ep�@e/@d�j@d(�@c�m@c��@c@b�\@b=q@bJ@a�@a��@a�7@aG�@a�@`��@`��@`r�@`A�@` �@`b@_��@_�@_�P@^�+@^@]��@]?}@\��@\�@\�D@\j@\9X@[�@Z��@Zn�@ZM�@ZM�@Z-@Y��@Y%@XĜ@X��@X�u@X1'@Xb@W�@W�P@V�y@V5?@U�@U/@T�/@T�@Sƨ@SC�@R��@R�!@Q��@Q�7@QG�@Q%@P�`@P��@P�@Pr�@P1'@O�@O��@Ol�@O;d@O
=@N��@Nv�@N$�@M��@M�-@MO�@L�@L�D@L�D@Lz�@LI�@K�
@Kt�@KS�@KC�@KdZ@K33@J�@J~�@J=q@JJ@Ix�@I�@H��@H��@HĜ@H�9@H�@HA�@G�;@GK�@Fȴ@F�+@Fff@E�T@E@E@E�-@E��@Ep�@D�@D��@DZ@D�@C�F@CdZ@C33@C@B��@B�\@B~�@B^5@BM�@A��@@�`@@r�@@ �@?�w@?�P@?l�@?+@>��@>�@>��@>v�@>{@=�-@=�@<��@<�j@<z�@<1@;�
@;�@;"�@:��@:��@:��@:~�@:M�@9�#@9x�@9G�@9%@8Ĝ@8��@8bN@81'@8 �@7�;@7�@7l�@7K�@7K�@7;d@7+@7�@6��@6E�@5�@5�-@5O�@5�@4�@4�j@4�D@4j@4(�@3��@3@2��@2~�@2-@1��@1hs@1&�@1%@0�9@0A�@0 �@0b@/��@/��@/l�@/�@.�y@.ȴ@.�+@.$�@-�@-��@-`B@-/@,�@,z�@,Z@,1@+�@+@*��@*��@*^5@*�@)��@)�#@)�#@)��@)��@)G�@)�@(��@(Ĝ@(r�@( �@(b@(  @(  @'�@'|�@'K�@'+@&��@&5?@&@%�T@%��@%�-@%�@%`B@%�@$��@$�@$�D@$Z@$9X@$9X@$�@#��@#ƨ@#t�@#33@#"�@#o@#o@#o@"�@"��@"�\@"M�@"J@!��@!x�@!G�@!%@ ��@ �`@ Ĝ@ �@ r�@ Q�@  �@ b@��@�P@\)@�@��@�@ȴ@��@v�@{@�@�@�T@�T@��@�-@�@O�@/@�/@��@j@j@j@I�@(�@1@�
@��@C�@"�@�@��@�\@�\@^5@-@��@�^@��@��@�7@hs@hs@X@%@�9@��@�u@�u@�@�@ �@��@�@|�@+@�@��@�R@��@V@5?@@��@��@p�@?}@?}@V@�@��@�j@��@Z@I�@9X@�@�m@ƨ@��@dZ@C�@C�@33@"�@"�@"�@o@�H@��@��@�\@^5@-@J@��@�@��@��@��@��@�7@X@7L@�@��@�`@Ĝ@�u@bN@1'@�@��@�@��@��@|�@K�@;d@��@�R@��@v�@ff@V@@�@p�@`B@V@�/@�@z�@j@j@�@�
@��@dZ@"�@
�!@
M�@	��@	�#@	��@	X@	7L@	&�@��@�9@��@�u@�@bN@ �@  @�;@��@�w@K�@�@
=@
=@
=@��@v�@v�@ff@$�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AӉ7Aә�Aӝ�Aӝ�Aӝ�Aӡ�Aӣ�Aӧ�Aӧ�Aӣ�Aӟ�Aӝ�Aӛ�AӓuA�hsA�I�A���A�E�A�^5A�Q�A�(�A�l�A���A�z�A���Aç�A�1A�I�A���A���A��-A�XA���A�t�A�A�XA���A�O�A��A�"�A��!A�oA�jA��#A��A��mA���A�1A�$�A�A�A�z�A��yA�S�A��^A�"�A��hA���A�bNA��A��A���A�jA�{A�ffA�%A�hsA��^A�=qA�  A���A�"�A�oA��A���A�A��9A�v�A�O�A�bNA�O�A�(�A�"�A�p�A�l�A��hA��FA�\)A��A���A��A��uA��A�{A���A�XA�hsA���A��A�hsA�JA��RA�1A�1A���A�v�A�A��9A��hA�hsA�1'A�JA�wAA}��A{��Av��Asp�ArE�Ao�Am�AmK�Al�9Ak��Ai�Ag%AfbAet�AeVAd-Ac�^Ac\)Ab��Abv�AaG�A_O�A]A[S�AY?}AW��AV{AUx�AT��ASO�AQ�#AP��AP�AO\)AM\)AL-AL  AK��AK%AJ1AGdZAE��ADZAC��AC7LAC%AB�`ABĜABZAA�
A@�9A?K�A>�jA=S�A;�wA;dZA9�;A8E�A6��A5��A57LA4A�A3�PA3%A2jA1�A1oA0M�A/oA-��A-|�A-33A,1'A+%A*z�A)�^A(��A'��A&��A&��A&�DA&  A%x�A%�A#t�A"Q�A!G�A ��A ��A (�A�PA�DAx�A�yA��AȴAZA�A?}A�AO�AVA/A�mAS�A��AbNA�
A7LA�DAO�A��A��A;dA	�TA	VA��AM�A��A��AhsA��A1A��A��A�!A�TA�PAAffA5?AJA�
A��AXA 1'@�M�@���@�K�@���@��`@�K�@�V@��@��@�`B@�%@�9X@�dZ@�n�@��#@��@��@�;d@��@�$�@��#@�@�V@�^5@�G�@㕁@�33@��@���@��m@��@ݺ^@��`@�+@ץ�@�hs@�z�@�1@��@Η�@���@�E�@Ɂ@ǍP@�V@��@�I�@å�@���@��H@�{@�hs@��@���@�33@�^5@�p�@��`@��@��w@�33@��@���@�J@��-@��@�%@��u@�r�@�1'@�@�@�b@�+@�$�@��;@�ff@��h@�j@�+@�V@�O�@���@��@��m@�
=@�M�@�E�@�$�@��T@��@��@��;@�S�@���@��R@�^5@�@�hs@��@��`@��9@�r�@���@��@��\@�V@�@�?}@���@�Ĝ@��u@��;@�|�@���@�7L@�&�@�V@��9@� �@���@��w@�C�@��y@���@��@��h@��@�hs@�`B@�&�@�z�@���@�ƨ@���@��@�|�@�\)@�"�@�@��@��@��R@�v�@�J@��h@�`B@���@���@�;d@���@�n�@�$�@��#@���@��@�/@��@��u@�r�@�  @���@��@�l�@�dZ@�\)@�S�@�K�@�C�@�"�@��y@�ff@��#@���@��@�`B@�?}@��@��@� �@�ƨ@�@���@��\@�~�@�v�@�M�@�{@�@��@���@�@��-@���@��h@�p�@�?}@�V@���@��`@���@�  @���@��w@��w@���@�C�@���@��H@��H@���@��!@���@�v�@��@��@���@��@�/@�V@���@��@�z�@�z�@�Z@�  @�@�P@K�@�@~E�@}�T@}�@|�@|1@{ƨ@{dZ@{o@z�!@z�@y��@y��@y�7@x��@x�u@xb@w\)@w;d@v��@v�@vȴ@vE�@u�@u?}@t��@s��@sC�@r��@rM�@rJ@q�#@q��@q��@q��@qhs@q%@pĜ@p�u@pbN@pA�@pb@o�w@o�P@o\)@n�@n�R@nff@n5?@n5?@n@n@m�T@m�-@m�-@m/@l�@l��@l��@l9X@k�@j�@j��@jM�@i�#@ix�@h��@h �@g�@g�w@gl�@g+@g
=@f��@fV@f$�@e��@ep�@e/@d�j@d(�@c�m@c��@c@b�\@b=q@bJ@a�@a��@a�7@aG�@a�@`��@`��@`r�@`A�@` �@`b@_��@_�@_�P@^�+@^@]��@]?}@\��@\�@\�D@\j@\9X@[�@Z��@Zn�@ZM�@ZM�@Z-@Y��@Y%@XĜ@X��@X�u@X1'@Xb@W�@W�P@V�y@V5?@U�@U/@T�/@T�@Sƨ@SC�@R��@R�!@Q��@Q�7@QG�@Q%@P�`@P��@P�@Pr�@P1'@O�@O��@Ol�@O;d@O
=@N��@Nv�@N$�@M��@M�-@MO�@L�@L�D@L�D@Lz�@LI�@K�
@Kt�@KS�@KC�@KdZ@K33@J�@J~�@J=q@JJ@Ix�@I�@H��@H��@HĜ@H�9@H�@HA�@G�;@GK�@Fȴ@F�+@Fff@E�T@E@E@E�-@E��@Ep�@D�@D��@DZ@D�@C�F@CdZ@C33@C@B��@B�\@B~�@B^5@BM�@A��@@�`@@r�@@ �@?�w@?�P@?l�@?+@>��@>�@>��@>v�@>{@=�-@=�@<��@<�j@<z�@<1@;�
@;�@;"�@:��@:��@:��@:~�@:M�@9�#@9x�@9G�@9%@8Ĝ@8��@8bN@81'@8 �@7�;@7�@7l�@7K�@7K�@7;d@7+@7�@6��@6E�@5�@5�-@5O�@5�@4�@4�j@4�D@4j@4(�@3��@3@2��@2~�@2-@1��@1hs@1&�@1%@0�9@0A�@0 �@0b@/��@/��@/l�@/�@.�y@.ȴ@.�+@.$�@-�@-��@-`B@-/@,�@,z�@,Z@,1@+�@+@*��@*��@*^5@*�@)��@)�#@)�#@)��@)��@)G�@)�@(��@(Ĝ@(r�@( �@(b@(  @(  @'�@'|�@'K�@'+@&��@&5?@&@%�T@%��@%�-@%�@%`B@%�@$��@$�@$�D@$Z@$9X@$9X@$�@#��@#ƨ@#t�@#33@#"�@#o@#o@#o@"�@"��@"�\@"M�@"J@!��@!x�@!G�@!%@ ��@ �`@ Ĝ@ �@ r�@ Q�@  �@ b@��@�P@\)@�@��@�@ȴ@��@v�@{@�@�@�T@�T@��@�-@�@O�@/@�/@��@j@j@j@I�@(�@1@�
@��@C�@"�@�@��@�\@�\@^5@-@��@�^@��@��@�7@hs@hs@X@%@�9@��@�u@�u@�@�@ �@��@�@|�@+@�@��@�R@��@V@5?@@��@��@p�@?}@?}@V@�@��@�j@��@Z@I�@9X@�@�m@ƨ@��@dZ@C�@C�@33@"�@"�@"�@o@�H@��@��@�\@^5@-@J@��@�@��@��@��@��@�7@X@7L@�@��@�`@Ĝ@�u@bN@1'@�@��@�@��@��@|�@K�@;d@��@�R@��@v�@ff@V@@�@p�@`B@V@�/@�@z�@j@j@�@�
@��@dZ@"�@
�!@
M�@	��@	�#@	��@	X@	7L@	&�@��@�9@��@�u@�@bN@ �@  @�;@��@�w@K�@�@
=@
=@
=@��@v�@v�@ff@$�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB �B �B �B �B �B �B �B �B �B!�B!�B"�B#�B$�B+B0!B;dBdZB�
B�B��B��B��B��B+BoB�B�BJB
=B
=BDBVBhB�B$�B,B2-B9XB<jB9XB:^BR�B^5BffBl�Bl�BhsBjBn�BhsBe`Be`BhsBs�Bt�Bo�B`BBT�BP�BH�B7LB)�B)�B)�B-B/B'�B#�B�BuB%B�B�fB�mB�sB�fB�;B��BƨB�?B��B��B�PB� Bx�Bs�Bn�BhsB`BBW
BN�B?}B)�B�B	7B
��B
�B
�yB
�HB
��B
��B
�JB
�+B
�B
}�B
y�B
w�B
u�B
q�B
o�B
l�B
ffB
\)B
L�B
,B
uB

=B	��B	�B	�yB	�`B	�5B	��B	��B	�^B	�LB	�9B	�B	�B	��B	��B	��B	��B	�PB	~�B	s�B	gmB	]/B	T�B	P�B	L�B	D�B	<jB	6FB	1'B	+B	 �B	�B	�B	�B	oB	JB	B��B��B��B��B��B��B��B�B�B�B�B�B�mB�TB�HB�#B�B��B��BɺBŢBB�}B�jB�^B�LB�9B�!B�B��B��B��B��B��B��B��B�{B�oB�hB�bB�VB�JB�=B�%B�B� B}�B|�Bz�Bx�Bu�Br�Bq�Bn�Bk�BjBhsBffBdZBaHB^5B[#BYBW
BVBT�BR�BQ�BO�BL�BI�BF�BB�B@�B?}B>wB=qB<jB;dB:^B9XB9XB8RB7LB5?B49B49B33B33B2-B2-B2-B2-B0!B.B,B(�B&�B%�B%�B$�B#�B#�B#�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B"�B#�B$�B#�B"�B$�B#�B#�B"�B!�B!�B �B!�B"�B!�B �B �B�B�B�B�B!�B �B�B�B�B�B�B�B!�B#�B(�B+B-B/B/B0!B0!B0!B0!B0!B1'B33B33B6FB8RB8RB9XB:^B<jB>wB?}BG�BF�BF�BD�BF�BK�BM�BQ�BQ�BR�BT�BW
BZBZBZB[#B]/BaHBbNBdZBe`BffBhsBk�Bl�Bm�Bn�Bo�Bo�Bs�Bv�By�Bz�B{�B� B�B�B�B�%B�+B�DB�uB�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�-B�-B�3B�9B�?B�FB�FB�LB�RB�XB�jB�}B��BB��B��B��B��B�B�B�#B�)B�;B�HB�TB�ZB�mB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	+B	
=B	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	"�B	#�B	%�B	&�B	&�B	)�B	/B	2-B	2-B	33B	5?B	8RB	;dB	;dB	;dB	<jB	=qB	=qB	?}B	B�B	D�B	F�B	H�B	K�B	K�B	M�B	P�B	P�B	P�B	Q�B	T�B	T�B	W
B	XB	XB	\)B	^5B	_;B	cTB	e`B	ffB	hsB	jB	k�B	n�B	o�B	p�B	p�B	r�B	t�B	v�B	w�B	w�B	x�B	y�B	y�B	}�B	~�B	�B	�%B	�DB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�?B	�LB	�^B	�jB	�jB	�qB	�wB	�wB	��B	ÖB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�;B	�HB	�HB	�TB	�ZB	�`B	�`B	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
	7B
	7B
	7B

=B
DB
JB
JB
JB
PB
VB
\B
bB
bB
bB
oB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
'�B
'�B
'�B
(�B
)�B
)�B
+B
+B
,B
,B
,B
,B
,B
-B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
8RB
8RB
9XB
9XB
:^B
;dB
<jB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
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
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
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
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
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
gmB
gmB
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
iyB
iyB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
r�B
r�B
s�B
s�B
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
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B$�B+B7LBcTB��B�B�B��B��B��B%BhB�B�B%BBB%B1BJBhB �B(�B.B49B6FB5?B7LBM�BXBbNBhsBgmBdZBffBiyBcTB`BB`BBcTBn�Bp�Bl�B]/BN�BK�BD�B33B#�B#�B$�B'�B(�B!�B�B�B\BB�B�BB�HB�TB�TB�#B��BB�'B��B��B�7Bz�Br�Bn�BiyBcTB[#BQ�BK�B<jB%�BoBB
��B
�B
�ZB
�BB
��B
��B
�%B
�B
~�B
w�B
s�B
q�B
o�B
k�B
iyB
gmB
bNB
YB
K�B
(�B
PB
+B	��B	�sB	�ZB	�HB	�#B	��B	�dB	�9B	�'B	�B	��B	��B	��B	��B	��B	��B	�=B	z�B	o�B	cTB	XB	N�B	K�B	G�B	?}B	7LB	1'B	,B	&�B	�B	�B	{B	hB	VB	
=B��B��B�B�B�B�B�B�B�B�B�B�yB�mB�TB�/B�/B�
B��B��BƨBĜB��B�jB�^B�LB�9B�-B�!B�B��B��B��B��B��B��B��B�uB�VB�JB�DB�DB�7B�+B�%B�B|�By�Bw�Bw�Bu�Bs�Bp�Bm�Bl�BiyBe`Be`BcTBaHB`BB\)BYBVBR�BQ�BO�BO�BM�BL�BK�BH�BD�BB�B=qB;dB9XB9XB7LB6FB5?B5?B33B33B2-B33B0!B.B/B.B-B,B,B,B-B,B(�B&�B$�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B$�B'�B(�B(�B)�B)�B)�B)�B)�B+B-B-B0!B2-B2-B49B5?B7LB9XB:^BC�BA�B@�B?}BA�BE�BH�BK�BK�BM�BN�BP�BS�BS�BS�BVBW
B[#B\)B^5B_;B`BBbNBe`BffBgmBhsBiyBiyBm�Bp�Bs�Bt�Bu�By�B{�B{�B}�B� B�B�%B�JB�VB�VB�bB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�!B�'B�-B�3B�FB�XB�^B�qBƨBɺB��B��B��B��B��B�B�B�#B�/B�5B�HB�`B�`B�fB�fB�fB�mB�mB�mB�sB�B�B�B��B��B��B��B��B��B	B	B	JB	bB	hB	hB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	#�B	(�B	,B	,B	-B	/B	2-B	5?B	5?B	5?B	6FB	7LB	7LB	9XB	<jB	>wB	@�B	B�B	E�B	E�B	G�B	J�B	J�B	J�B	K�B	N�B	N�B	P�B	Q�B	Q�B	VB	XB	YB	]/B	_;B	`BB	bNB	dZB	e`B	hsB	iyB	jB	jB	l�B	n�B	p�B	q�B	q�B	r�B	s�B	s�B	w�B	x�B	|�B	� B	�B	�7B	�JB	�VB	�\B	�bB	�hB	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�9B	�FB	�FB	�LB	�RB	�RB	�^B	�qB	�}B	��B	B	ÖB	ÖB	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�/B	�5B	�;B	�;B	�;B	�BB	�NB	�ZB	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
	7B

=B

=B

=B
JB
PB
PB
VB
VB
VB
VB
\B
bB
hB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
,B
-B
-B
-B
-B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
33B
33B
49B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
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
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
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
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
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
gmB
gmB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
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
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 =ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=ix�=ix�=ix�=ix�=ix�=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=m�h=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=ix�=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=aG�=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`B=e`BPRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects CTD thermal lag (CTL) viz. Johnson et al, 2007, JAOT, effects of pressure adjustments, and PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                  PADJ REPORTED_SURFACE_PRESSURE =0.08 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTL alpha = 0.021 & tau = 21 s with error equal to |correction| and for OW r = 0.9998 (+/-0.0014), vertically averaged dS = -0.006 (+/-0.055)                                                                                                                   Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            After pressure and cell thermal lag correction of salinity values, OW correction estimated using mapping scales of 8 & 4 long. and 4 & 2 lat., no PV constraint, and decorrelation time scale of 10 years.                                                      202307212300282023072123002820230721230028  AO  ARCAADJP                                                                    20200708110026    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200708110026  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200708110026  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20230712105800  QC  PRES            @�ffD� G�O�                PM  ARSQCTM V1.1                                                                20230712105800  QC  PSAL            @�ffD� G�O�                PM  ARSQCOWGV1.1CTD_2021v2 + Argo_2021v03                                       20230721230927  IP                  G�O�G�O�G�O�                