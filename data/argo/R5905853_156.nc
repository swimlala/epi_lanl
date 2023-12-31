CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-05-19T12:43:54Z creation;2023-05-19T12:43:55Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230519124354  20230519125703  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�,S���1   @�,T�Z��@/O�;�b�l�C��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@y��@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�ffB���B���B�  B�  B���B���B�  B�33B���B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"fD"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @4z�@tz�@�=q@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B@zBG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
B�=pB�p�B£�B��
B��
Bϣ�Bӣ�B��
B�
=Bߣ�B��B��
B��
B��
B��
B��
B��
B��
C�C�C�C�C	�C�C�C�CC��C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-��C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CHCJCK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce��Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�DGDz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D"GD"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DB�GDB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�f�A�d�A�h�A�b�A�b�A�W?A�/�A��QAȜ�AȚkA�|�A�J�A���Aǰ!AǤtA���A��dAǉA�B�A��"AƋDA�k�A�W
A��A��AżAŅ�A�uA�F�A�o�A�|�A�jA�U�A�v�AƱ�A�ޞAƿ}AƮ}AƼjA���A��;A�бAƴA�v�A���AVA�iyA���A�~�A��A�MA��}A�,=A�|�A��)A���A��A�y�A�ѷA�LdA�JXA�xlA�^�A�T�A���A��yA���A~��Az�;AwC�Au�Ar�Aj�"Ag��Ae�2Ac�Abu�Aa͟A^�pA\�SAZ��AXO�AUȴAT�$AS iAN>�AK͟AH��AF(�ABg8A=,�A:qA8A�A6JA51'A4`�A3J#A21�A1یA1&A/y�A+�EA(��A(ݘA)�|A*R�A)r�A'��A$��A"��A!�A �A �A1AR�AXyA��A�rA�9A��AsA�AZA�A!�A�0AGA�?AXAOAB[AH�A|AC�A��Ag�AVmA[�AW?A_A��Av�A��A�A(�A
��A
+kA	ƨA	m�A��A~�AĜA�A�.A��A��A;A��A��A��A��A��A��AN<AVA�1AJ�A��A�	A%FA ��A Z�A &�A (@��;@��	@�=�@�g8@�E�@��@���@�Q�@�A @�%�@��f@��j@�D�@�O@�t�@��j@�0�@�A�@�,�@�ԕ@� i@�@�'R@�Ft@��@�s�@�Y@� i@��H@�z@��@�4@��A@�Z�@�Ft@���@�}�@�_@�U�@�h@�S�@��Z@��@�V�@�J�@�b@�*0@�r�@�8@��@��@ڴ9@�=q@�S�@��@��r@�O�@��@�u�@��&@�IR@ԟ�@�e@� \@��/@ҡb@�|�@�GE@��@�?}@�S@У�@���@Ϝ@�W?@�Y@��/@Γu@��@�b�@�>�@�Y@�ȴ@�5?@˗$@�F�@�@���@�~�@�8�@�B�@��B@ȗ�@��)@�P�@���@Ƅ�@��@ńM@�a�@�T�@�>�@��@Ě@�&�@Î�@�W?@�5�@�=@�.I@�7L@ù�@�[W@�(�@���@��)@¶�@��m@�6z@���@�]d@��@��@�o @�9�@��/@���@�Ɇ@���@�oi@�/�@��z@�]�@�o@�ں@��4@��b@���@�J@���@�+�@��A@��}@�{J@�q@���@���@���@�;d@���@�6�@��
@���@��@�4@���@���@�Mj@���@���@�$�@���@�V@��c@��e@�M@�{@��
@�J�@��	@�q�@���@��.@��@��'@�n/@��@��u@��@��w@���@�s@�"�@���@��A@���@��!@���@�^5@���@�p�@�7L@�5�@�-w@��	@��@�u�@�q@��D@�:�@��@�^�@� i@���@��}@��L@��@��@��V@��@�Mj@�*0@��s@�PH@�"h@��+@���@�l�@��@�Ɇ@�y>@�E�@��'@�o @�Dg@��"@���@�Ta@�"h@���@���@��"@�qv@�8@��P@��@��<@��@�u�@�5?@��]@��&@���@�S@���@���@�j@�
�@�l�@�
=@���@��@���@��O@��@���@�E�@��K@�c@�Dg@�9�@�
=@���@�^5@� �@��6@��"@�)_@��@���@�q@�C�@�@���@��z@���@�(�@��,@���@�l"@�%�@��@���@��@�c�@��@��L@�6@��z@�o@��@��P@��@��`@���@�Ov@�  @��@���@�k�@�S�@�8@���@���@�}�@�/@�/@�?}@�@��[@�a|@�	�@��9@��h@���@��]@��'@���@�6z@��"@��O@��@���@���@�]�@�!-@���@�7�@��@��@��g@��)@��@�c@��K@���@��e@���@���@�U2@��@���@��T@��C@�<6@�(@�o@���@��y@���@��\@�_�@���@��H@���@��@��~@�L�@�'�@��@���@��+@�\�@�(�@��@dZ@~�A@~	@}��@}c�@|�|@|�@|7�@{��@{y�@{�@zp;@y�D@y�#@y��@yV@x�)@xj@w�K@wW?@w9�@w.I@vߤ@v��@u�.@u�-@u^�@u+�@t��@t��@tw�@t4n@s��@sg�@r�@r�x@r_�@r
�@qm]@q \@p��@o�F@o��@o9�@n�@nOv@m�9@m��@mX@l��@l~(@lH@l,=@l1'@l �@k�:@k�@j�H@j��@ji�@jTa@j?@i�@i[W@i;@h�@h�9@h]d@g��@g"�@f�@f��@f��@f	@es�@eDg@d��@d�@b�!@b-@a�D@a�@aG�@a \@`�`@`w�@`%�@_� @_|�@_o@^�!@^\�@]��@]-w@\�e@\�@[�V@[�@Zp;@ZE�@Y�M@XbN@W�$@V��@V$�@U��@UQ�@T��@T9X@S��@S4�@R�r@RV@R	@Q��@Q�@QF@P��@P4n@O�@O�[@O��@O;d@N�R@Nn�@N($@N	@N�@N
�@M��@Mp�@L�@Ly>@LI�@K�r@K��@K��@KA�@Jv�@JL0@J@�@J6�@I��@I�@I�"@Ie,@I�@H��@H-�@H�@G��@G�K@G��@G�:@Gy�@GS�@G1�@G�@F�"@F�c@F͟@F�6@F_�@E�@E�@Ec@E<6@D�E@Doi@C��@CP�@C=@C�@B��@B��@Bȴ@B��@B��@B��@B}V@B^5@B	@A�=@A:�@A%@@�E@@�@@N�@@�@?��@?�F@?�	@?y�@?C�@>�M@>q�@>;�@>e@>u@=ԕ@=c�@=�@<�@<|�@<bN@<`�@<@;��@;O@:�@:��@:{�@:6�@9��@9��@98�@9�@9�@8��@8Z@8�@7��@7��@74�@6��@6	@5ԕ@5|@5Vm@5`B@5O�@4��@3�@3n/@3H�@36z@3�@2��@2��@2C�@1��@1�@1+�@0�_@0z�@0�@/�f@/]�@.�"@.�L@.a|@.J@-@-��@-s�@-`B@-q@,�v@,Ɇ@,r�@,:�@, �@+�@+��@+�P@+O@+�@*�<@*��@*h
@*R�@*($@)��@)�t@)a�@) \@(�5@(��@(|�@(1'@(@(G@'��@'�@'E9@'�@&�!@&v�@&YK@&@�@&�@%�"@%7L@%@$�	@$��@$��@$r�@$S�@$�@#ݘ@#��@#Mj@#�@"��@"~�@"R�@"5?@"�@!�9@!��@!��@!J�@!q@ �5@ �?@ ��@ ��@ ]d@ 9X@ 1@�@�a@��@W?@>�@(@��@҉@��@i�@J�@)�@�)@�'@S&@@@�)@�9@�I@�@q@$@��@��@�{@E9@+@�@�,@�R@�1@v�@h
@Q@6�@�@�>@�^@�=@��@F@7L@@��@��@�e@��@[�@7@@�@�;@��@a@K�@6z@�8@ں@��@n�@L0@.�@�@�@�@�9@�3@�7@J�@!�@�@�@M@G@�F@�f@E9@,�@$t@�]@��@O@�'@o @G�@ \@��@ѷ@�U@��@�e@�@"h@b@��@�@��@iD@/�@C@�,@��@�r@��@}V@c @�@�d@�=@�@rG@O�@#�@�@��@�4@��@�.@V�@/�@�@�a@�f@iD@6z@
�@
ߤ@
��@
��@
c @
@�@
+k@
u@	�@	��@	�M@	m]@	a�@	X@	8�@�@�@>B@�@�]111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�f�A�d�A�h�A�b�A�b�A�W?A�/�A��QAȜ�AȚkA�|�A�J�A���Aǰ!AǤtA���A��dAǉA�B�A��"AƋDA�k�A�W
A��A��AżAŅ�A�uA�F�A�o�A�|�A�jA�U�A�v�AƱ�A�ޞAƿ}AƮ}AƼjA���A��;A�бAƴA�v�A���AVA�iyA���A�~�A��A�MA��}A�,=A�|�A��)A���A��A�y�A�ѷA�LdA�JXA�xlA�^�A�T�A���A��yA���A~��Az�;AwC�Au�Ar�Aj�"Ag��Ae�2Ac�Abu�Aa͟A^�pA\�SAZ��AXO�AUȴAT�$AS iAN>�AK͟AH��AF(�ABg8A=,�A:qA8A�A6JA51'A4`�A3J#A21�A1یA1&A/y�A+�EA(��A(ݘA)�|A*R�A)r�A'��A$��A"��A!�A �A �A1AR�AXyA��A�rA�9A��AsA�AZA�A!�A�0AGA�?AXAOAB[AH�A|AC�A��Ag�AVmA[�AW?A_A��Av�A��A�A(�A
��A
+kA	ƨA	m�A��A~�AĜA�A�.A��A��A;A��A��A��A��A��A��AN<AVA�1AJ�A��A�	A%FA ��A Z�A &�A (@��;@��	@�=�@�g8@�E�@��@���@�Q�@�A @�%�@��f@��j@�D�@�O@�t�@��j@�0�@�A�@�,�@�ԕ@� i@�@�'R@�Ft@��@�s�@�Y@� i@��H@�z@��@�4@��A@�Z�@�Ft@���@�}�@�_@�U�@�h@�S�@��Z@��@�V�@�J�@�b@�*0@�r�@�8@��@��@ڴ9@�=q@�S�@��@��r@�O�@��@�u�@��&@�IR@ԟ�@�e@� \@��/@ҡb@�|�@�GE@��@�?}@�S@У�@���@Ϝ@�W?@�Y@��/@Γu@��@�b�@�>�@�Y@�ȴ@�5?@˗$@�F�@�@���@�~�@�8�@�B�@��B@ȗ�@��)@�P�@���@Ƅ�@��@ńM@�a�@�T�@�>�@��@Ě@�&�@Î�@�W?@�5�@�=@�.I@�7L@ù�@�[W@�(�@���@��)@¶�@��m@�6z@���@�]d@��@��@�o @�9�@��/@���@�Ɇ@���@�oi@�/�@��z@�]�@�o@�ں@��4@��b@���@�J@���@�+�@��A@��}@�{J@�q@���@���@���@�;d@���@�6�@��
@���@��@�4@���@���@�Mj@���@���@�$�@���@�V@��c@��e@�M@�{@��
@�J�@��	@�q�@���@��.@��@��'@�n/@��@��u@��@��w@���@�s@�"�@���@��A@���@��!@���@�^5@���@�p�@�7L@�5�@�-w@��	@��@�u�@�q@��D@�:�@��@�^�@� i@���@��}@��L@��@��@��V@��@�Mj@�*0@��s@�PH@�"h@��+@���@�l�@��@�Ɇ@�y>@�E�@��'@�o @�Dg@��"@���@�Ta@�"h@���@���@��"@�qv@�8@��P@��@��<@��@�u�@�5?@��]@��&@���@�S@���@���@�j@�
�@�l�@�
=@���@��@���@��O@��@���@�E�@��K@�c@�Dg@�9�@�
=@���@�^5@� �@��6@��"@�)_@��@���@�q@�C�@�@���@��z@���@�(�@��,@���@�l"@�%�@��@���@��@�c�@��@��L@�6@��z@�o@��@��P@��@��`@���@�Ov@�  @��@���@�k�@�S�@�8@���@���@�}�@�/@�/@�?}@�@��[@�a|@�	�@��9@��h@���@��]@��'@���@�6z@��"@��O@��@���@���@�]�@�!-@���@�7�@��@��@��g@��)@��@�c@��K@���@��e@���@���@�U2@��@���@��T@��C@�<6@�(@�o@���@��y@���@��\@�_�@���@��H@���@��@��~@�L�@�'�@��@���@��+@�\�@�(�@��@dZ@~�A@~	@}��@}c�@|�|@|�@|7�@{��@{y�@{�@zp;@y�D@y�#@y��@yV@x�)@xj@w�K@wW?@w9�@w.I@vߤ@v��@u�.@u�-@u^�@u+�@t��@t��@tw�@t4n@s��@sg�@r�@r�x@r_�@r
�@qm]@q \@p��@o�F@o��@o9�@n�@nOv@m�9@m��@mX@l��@l~(@lH@l,=@l1'@l �@k�:@k�@j�H@j��@ji�@jTa@j?@i�@i[W@i;@h�@h�9@h]d@g��@g"�@f�@f��@f��@f	@es�@eDg@d��@d�@b�!@b-@a�D@a�@aG�@a \@`�`@`w�@`%�@_� @_|�@_o@^�!@^\�@]��@]-w@\�e@\�@[�V@[�@Zp;@ZE�@Y�M@XbN@W�$@V��@V$�@U��@UQ�@T��@T9X@S��@S4�@R�r@RV@R	@Q��@Q�@QF@P��@P4n@O�@O�[@O��@O;d@N�R@Nn�@N($@N	@N�@N
�@M��@Mp�@L�@Ly>@LI�@K�r@K��@K��@KA�@Jv�@JL0@J@�@J6�@I��@I�@I�"@Ie,@I�@H��@H-�@H�@G��@G�K@G��@G�:@Gy�@GS�@G1�@G�@F�"@F�c@F͟@F�6@F_�@E�@E�@Ec@E<6@D�E@Doi@C��@CP�@C=@C�@B��@B��@Bȴ@B��@B��@B��@B}V@B^5@B	@A�=@A:�@A%@@�E@@�@@N�@@�@?��@?�F@?�	@?y�@?C�@>�M@>q�@>;�@>e@>u@=ԕ@=c�@=�@<�@<|�@<bN@<`�@<@;��@;O@:�@:��@:{�@:6�@9��@9��@98�@9�@9�@8��@8Z@8�@7��@7��@74�@6��@6	@5ԕ@5|@5Vm@5`B@5O�@4��@3�@3n/@3H�@36z@3�@2��@2��@2C�@1��@1�@1+�@0�_@0z�@0�@/�f@/]�@.�"@.�L@.a|@.J@-@-��@-s�@-`B@-q@,�v@,Ɇ@,r�@,:�@, �@+�@+��@+�P@+O@+�@*�<@*��@*h
@*R�@*($@)��@)�t@)a�@) \@(�5@(��@(|�@(1'@(@(G@'��@'�@'E9@'�@&�!@&v�@&YK@&@�@&�@%�"@%7L@%@$�	@$��@$��@$r�@$S�@$�@#ݘ@#��@#Mj@#�@"��@"~�@"R�@"5?@"�@!�9@!��@!��@!J�@!q@ �5@ �?@ ��@ ��@ ]d@ 9X@ 1@�@�a@��@W?@>�@(@��@҉@��@i�@J�@)�@�)@�'@S&@@@�)@�9@�I@�@q@$@��@��@�{@E9@+@�@�,@�R@�1@v�@h
@Q@6�@�@�>@�^@�=@��@F@7L@@��@��@�e@��@[�@7@@�@�;@��@a@K�@6z@�8@ں@��@n�@L0@.�@�@�@�@�9@�3@�7@J�@!�@�@�@M@G@�F@�f@E9@,�@$t@�]@��@O@�'@o @G�@ \@��@ѷ@�U@��@�e@�@"h@b@��@�@��@iD@/�@C@�,@��@�r@��@}V@c @�@�d@�=@�@rG@O�@#�@�@��@�4@��@�.@V�@/�@�@�a@�f@iD@6z@
�@
ߤ@
��@
��@
c @
@�@
+k@
u@	�@	��@	�M@	m]@	a�@	X@	8�@�@�@>B@�@�]111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	-�B	-�B	-�B	-�B	-�B	-]B	,B	-�B	6B	;JB	=<B	>(B	A B	B�B	X+B	q�B	t�B	l�B	a�B	YB	M�B	O�B	P�B	R�B	TFB	TB	[�B	��B	��B	�1B	οB	��B	�EB	�mB	�B
�B
�B
&B
B
"4B
*eB
-wB
*�B
"hB
�B	��B	T�B	gB	hXB	vzB	q�B	utB	t9B	s�B	��B	�"B	�lB	�B	� B	�B	�9B	��B	�B	�OB	�+B	�[B	��B	k�B	[�B	L~B	A�B	8�B	5�B	,WB	,�B	)�B	%�B	6zB	>�B	9�B	72B	7�B	AUB	B�B	D�B	OB	R�B	L~B	=�B	"�B	_B��B�BޞB�5B�B��B��B��B	
�B��B� B��B�#B	�B	bB	,B	6B��B�eB	�B	�B	�B	#B	"�B	 �B	�B	�B	�B	�B	B	 'B	#�B	$�B	%zB	&�B	"�B	'�B	+�B	.�B	+�B	+B	(�B	,�B	.B	*B	*�B	/�B	8�B	=qB	AUB	I�B	MB	K�B	M�B	O�B	N�B	O�B	R B	Q�B	RTB	V�B	W
B	Z7B	Z�B	\xB	Z�B	\]B	d�B	k6B	n�B	tB	xB	�B	�B	��B	�B	��B	�2B	�1B	�B	��B	��B	�)B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�0B	�_B	��B	��B	��B	�2B	�zB	�ZB	��B	��B	�B	�@B	�tB	��B	��B	�
B	��B	��B	�B	�KB	�B	��B	��B	�eB	�B	�>B	�B	�zB	��B	��B	�FB	�\B	��B	��B	�jB	��B	��B	�B	�'B	�B	�DB	�)B	��B	��B	�B	�UB	��B	��B	�B	��B	�aB	��B	�B	�B	�TB	��B	��B	�?B	��B	��B	��B	�rB	��B	��B	�B	�dB	�B	�]B	� B	�B	��B	�[B	��B	�B	�SB	��B	��B	ɆB	�	B	�=B	ɺB	ɺB	�^B	�~B	�=B	��B	��B	�B	�"B	�B	�"B	�"B	�VB	οB	�B	ѝB	҉B	�&B	��B	�
B	�;B	�\B	�'B	��B	��B	��B	��B	�fB	��B	�mB	�B	�B	��B	�
B	��B	�0B	��B	�eB	�kB	�B	�B	�B	��B	��B	�B	�wB	�B	�/B	�)B	��B	�B	�kB	�]B	��B	��B	�B	�)B	�wB	�B	��B	��B	�/B	��B	�B	�5B	�B	��B	��B	�AB	�'B	�AB	�|B	�3B	�B	�B	�TB	�B	��B	�B	�B	�nB	��B	��B	��B	�+B	��B	��B	�LB	�fB	�fB	�B	�RB	��B	�B
  B
 �B
B
�B
 �B
  B
  B
;B
�B
�B
�B
GB
B
�B
?B
�B
3B
{B
3B
�B
B
B
�B
�B
�B
YB
0B
~B
�B
B
"B
�B
<B
VB
VB
<B
VB
(B
�B
.B
}B
 B
 B
 B
hB
�B
�B
�B
B
 B
oB
�B
�B
�B
�B
uB
uB
B
�B
�B
�B
�B
MB
�B
�B
�B
�B
B
B
B
B
�B
?B
$B
?B

B
YB
�B
+B
_B
yB
�B
B
eB
�B
B
B
7B
B
kB
	B
	B
�B
�B
�B
~B
�B
�B
�B
dB
IB
�B
�B
�B
B
5B
�B
�B
�B
;B
 vB
 �B
 �B
!HB
"�B
"�B
"hB
"B
 'B
 B
 \B
"B
#nB
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#B
#:B
#:B
#�B
$ZB
$�B
%�B
%�B
%�B
%�B
&LB
&fB
'RB
'�B
(>B
($B
)B
*B
)�B
(�B
(�B
)B
)B
)DB
)�B
)�B
*0B
*B
*0B
*�B
+B
+�B
,"B
,"B
,B
+�B
+�B
+�B
+B
+B
*�B
+B
+�B
+QB
+kB
,B
,"B
,=B
,�B
,�B
,�B
-�B
-�B
./B
.IB
.�B
/B
/�B
0B
0�B
1vB
1[B
1[B
2B
33B
2aB
2-B
2aB
3MB
3�B
3�B
3�B
4B
4�B
4�B
4�B
4�B
5B
5?B
5ZB
5�B
5�B
6+B
6zB
6�B
7LB
7B
7�B
8B
88B
8�B
9>B
8�B
9XB
9rB
9�B
:DB
:^B
:�B
:�B
;�B
<6B
;�B
<B
<�B
=�B
=�B
=�B
>�B
>�B
?�B
?cB
?�B
?HB
?�B
?�B
@ B
@B
@ B
@�B
@�B
AUB
B�B
DgB
D�B
D�B
E9B
D�B
C�B
C�B
DMB
D�B
DMB
D�B
EmB
E�B
E�B
E�B
FB
F%B
FYB
F�B
G�B
H1B
H�B
H�B
I�B
IRB
I�B
J	B
JXB
J�B
K^B
K�B
L0B
LdB
L�B
MPB
M�B
N<B
NVB
N<B
NVB
N�B
N�B
N�B
O(B
O�B
O�B
O�B
O�B
PB
PbB
P�B
R B
R�B
R�B
R�B
R�B
R�B
R�B
S&B
S�B
S�B
T,B
TFB
TFB
T�B
UMB
UgB
UgB
U�B
VB
V9B
V�B
V�B
W
B
W?B
W?B
WsB
WsB
W�B
W�B
W�B
W�B
X+B
X_B
X�B
YB
YB
YB
YKB
YB
Y�B
Y�B
ZB
ZB
ZkB
Z�B
Z�B
Z7B
ZkB
Z�B
[	B
[qB
[qB
[qB
[qB
[qB
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
]IB
]�B
]�B
]�B
]�B
]�B
^5B
^�B
^�B
^�B
^�B
_B
_;B
_�B
`BB
`\B
`vB
`'B
`�B
`�B
a-B
a�B
a�B
a�B
bB
a�B
a�B
a�B
b4B
bNB
b�B
cTB
c�B
c�B
c�B
dB
eFB
d�B
d�B
eB
ezB
e�B
e�B
fB
fLB
f�B
f�B
gRB
g�B
g�B
g�B
g�B
h>B
g�B
h�B
h�B
h�B
i*B
jKB
jB
jB
jKB
kB
k6B
kB
kB
kQB
k�B
k�B
k�B
k�B
lB
l=B
lqB
lqB
l�B
l�B
l�B
mB
m)B
mCB
mCB
mwB
mwB
m�B
m�B
nB
nIB
nIB
n�B
n�B
n�B
oB
o B
oOB
o�B
o�B
o�B
pUB
p�B
p�B
poB
p�B
p�B
q[B
qAB
q�B
q�B
q�B
q�B
rB
raB
raB
r|B
r�B
s3B
shB
s�B
s�B
s�B
s�B
tB
tB
tB
tnB
t�B
t�B
t�B
uB
uB
u%B
uZB
u�B
utB
u�B
u�B
vFB
v+B
vzB
v�B
v�B
wB
wfB
w�B
w�B
xB
xRB
x�B
x�B
x�B
y	B
y$B
y�B
y�B
y�B
y�B
y�B
zB
zDB
z^B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{dB
{�B
{�B
{�B
|B
|B
|6B
|jB
|�B
|�B
|�B
}"B
}"B
}<B
}"B
}VB
}qB
}�B
}�B
}�B
~B
~B
~]B
~�B
~�B
~�B
B
.B
HB
HB
HB
�B
�B
� B
�B
�OB
��B
�B
�UB
�UB
��B
��B
�oB
��B
��B
�[B
��B
��B
�B
�-B
�aB
��B
��B
��B
��B
��B
�3B
�3B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
��B
��B
�?B
�YB
��B
��B
��B
��B
��B
�_B
�_B
��B
��B
��B
�B
�B
�1B
��B
��B
�B
�B
�B
�7B
��B
��B
��B
��B
�	B
�XB
�=B
�XB
��B
��B
��B
��B
��B
�^B
��B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	-�B	-�B	-�B	-�B	-�B	-]B	,B	-�B	6B	;JB	=<B	>(B	A B	B�B	X+B	q�B	t�B	l�B	a�B	YB	M�B	O�B	P�B	R�B	TFB	TB	[�B	��B	��B	�1B	οB	��B	�EB	�mB	�B
�B
�B
&B
B
"4B
*eB
-wB
*�B
"hB
�B	��B	T�B	gB	hXB	vzB	q�B	utB	t9B	s�B	��B	�"B	�lB	�B	� B	�B	�9B	��B	�B	�OB	�+B	�[B	��B	k�B	[�B	L~B	A�B	8�B	5�B	,WB	,�B	)�B	%�B	6zB	>�B	9�B	72B	7�B	AUB	B�B	D�B	OB	R�B	L~B	=�B	"�B	_B��B�BޞB�5B�B��B��B��B	
�B��B� B��B�#B	�B	bB	,B	6B��B�eB	�B	�B	�B	#B	"�B	 �B	�B	�B	�B	�B	B	 'B	#�B	$�B	%zB	&�B	"�B	'�B	+�B	.�B	+�B	+B	(�B	,�B	.B	*B	*�B	/�B	8�B	=qB	AUB	I�B	MB	K�B	M�B	O�B	N�B	O�B	R B	Q�B	RTB	V�B	W
B	Z7B	Z�B	\xB	Z�B	\]B	d�B	k6B	n�B	tB	xB	�B	�B	��B	�B	��B	�2B	�1B	�B	��B	��B	�)B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�0B	�_B	��B	��B	��B	�2B	�zB	�ZB	��B	��B	�B	�@B	�tB	��B	��B	�
B	��B	��B	�B	�KB	�B	��B	��B	�eB	�B	�>B	�B	�zB	��B	��B	�FB	�\B	��B	��B	�jB	��B	��B	�B	�'B	�B	�DB	�)B	��B	��B	�B	�UB	��B	��B	�B	��B	�aB	��B	�B	�B	�TB	��B	��B	�?B	��B	��B	��B	�rB	��B	��B	�B	�dB	�B	�]B	� B	�B	��B	�[B	��B	�B	�SB	��B	��B	ɆB	�	B	�=B	ɺB	ɺB	�^B	�~B	�=B	��B	��B	�B	�"B	�B	�"B	�"B	�VB	οB	�B	ѝB	҉B	�&B	��B	�
B	�;B	�\B	�'B	��B	��B	��B	��B	�fB	��B	�mB	�B	�B	��B	�
B	��B	�0B	��B	�eB	�kB	�B	�B	�B	��B	��B	�B	�wB	�B	�/B	�)B	��B	�B	�kB	�]B	��B	��B	�B	�)B	�wB	�B	��B	��B	�/B	��B	�B	�5B	�B	��B	��B	�AB	�'B	�AB	�|B	�3B	�B	�B	�TB	�B	��B	�B	�B	�nB	��B	��B	��B	�+B	��B	��B	�LB	�fB	�fB	�B	�RB	��B	�B
  B
 �B
B
�B
 �B
  B
  B
;B
�B
�B
�B
GB
B
�B
?B
�B
3B
{B
3B
�B
B
B
�B
�B
�B
YB
0B
~B
�B
B
"B
�B
<B
VB
VB
<B
VB
(B
�B
.B
}B
 B
 B
 B
hB
�B
�B
�B
B
 B
oB
�B
�B
�B
�B
uB
uB
B
�B
�B
�B
�B
MB
�B
�B
�B
�B
B
B
B
B
�B
?B
$B
?B

B
YB
�B
+B
_B
yB
�B
B
eB
�B
B
B
7B
B
kB
	B
	B
�B
�B
�B
~B
�B
�B
�B
dB
IB
�B
�B
�B
B
5B
�B
�B
�B
;B
 vB
 �B
 �B
!HB
"�B
"�B
"hB
"B
 'B
 B
 \B
"B
#nB
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#B
#:B
#:B
#�B
$ZB
$�B
%�B
%�B
%�B
%�B
&LB
&fB
'RB
'�B
(>B
($B
)B
*B
)�B
(�B
(�B
)B
)B
)DB
)�B
)�B
*0B
*B
*0B
*�B
+B
+�B
,"B
,"B
,B
+�B
+�B
+�B
+B
+B
*�B
+B
+�B
+QB
+kB
,B
,"B
,=B
,�B
,�B
,�B
-�B
-�B
./B
.IB
.�B
/B
/�B
0B
0�B
1vB
1[B
1[B
2B
33B
2aB
2-B
2aB
3MB
3�B
3�B
3�B
4B
4�B
4�B
4�B
4�B
5B
5?B
5ZB
5�B
5�B
6+B
6zB
6�B
7LB
7B
7�B
8B
88B
8�B
9>B
8�B
9XB
9rB
9�B
:DB
:^B
:�B
:�B
;�B
<6B
;�B
<B
<�B
=�B
=�B
=�B
>�B
>�B
?�B
?cB
?�B
?HB
?�B
?�B
@ B
@B
@ B
@�B
@�B
AUB
B�B
DgB
D�B
D�B
E9B
D�B
C�B
C�B
DMB
D�B
DMB
D�B
EmB
E�B
E�B
E�B
FB
F%B
FYB
F�B
G�B
H1B
H�B
H�B
I�B
IRB
I�B
J	B
JXB
J�B
K^B
K�B
L0B
LdB
L�B
MPB
M�B
N<B
NVB
N<B
NVB
N�B
N�B
N�B
O(B
O�B
O�B
O�B
O�B
PB
PbB
P�B
R B
R�B
R�B
R�B
R�B
R�B
R�B
S&B
S�B
S�B
T,B
TFB
TFB
T�B
UMB
UgB
UgB
U�B
VB
V9B
V�B
V�B
W
B
W?B
W?B
WsB
WsB
W�B
W�B
W�B
W�B
X+B
X_B
X�B
YB
YB
YB
YKB
YB
Y�B
Y�B
ZB
ZB
ZkB
Z�B
Z�B
Z7B
ZkB
Z�B
[	B
[qB
[qB
[qB
[qB
[qB
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
]IB
]�B
]�B
]�B
]�B
]�B
^5B
^�B
^�B
^�B
^�B
_B
_;B
_�B
`BB
`\B
`vB
`'B
`�B
`�B
a-B
a�B
a�B
a�B
bB
a�B
a�B
a�B
b4B
bNB
b�B
cTB
c�B
c�B
c�B
dB
eFB
d�B
d�B
eB
ezB
e�B
e�B
fB
fLB
f�B
f�B
gRB
g�B
g�B
g�B
g�B
h>B
g�B
h�B
h�B
h�B
i*B
jKB
jB
jB
jKB
kB
k6B
kB
kB
kQB
k�B
k�B
k�B
k�B
lB
l=B
lqB
lqB
l�B
l�B
l�B
mB
m)B
mCB
mCB
mwB
mwB
m�B
m�B
nB
nIB
nIB
n�B
n�B
n�B
oB
o B
oOB
o�B
o�B
o�B
pUB
p�B
p�B
poB
p�B
p�B
q[B
qAB
q�B
q�B
q�B
q�B
rB
raB
raB
r|B
r�B
s3B
shB
s�B
s�B
s�B
s�B
tB
tB
tB
tnB
t�B
t�B
t�B
uB
uB
u%B
uZB
u�B
utB
u�B
u�B
vFB
v+B
vzB
v�B
v�B
wB
wfB
w�B
w�B
xB
xRB
x�B
x�B
x�B
y	B
y$B
y�B
y�B
y�B
y�B
y�B
zB
zDB
z^B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{B
{dB
{�B
{�B
{�B
|B
|B
|6B
|jB
|�B
|�B
|�B
}"B
}"B
}<B
}"B
}VB
}qB
}�B
}�B
}�B
~B
~B
~]B
~�B
~�B
~�B
B
.B
HB
HB
HB
�B
�B
� B
�B
�OB
��B
�B
�UB
�UB
��B
��B
�oB
��B
��B
�[B
��B
��B
�B
�-B
�aB
��B
��B
��B
��B
��B
�3B
�3B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
��B
��B
�?B
�YB
��B
��B
��B
��B
��B
�_B
�_B
��B
��B
��B
�B
�B
�1B
��B
��B
�B
�B
�B
�7B
��B
��B
��B
��B
�	B
�XB
�=B
�XB
��B
��B
��B
��B
��B
�^B
��B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230519124353  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230519124354  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230519124355  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230519124355                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230519124356  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230519124356  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230519125703                      G�O�G�O�G�O�                