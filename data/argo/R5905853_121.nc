CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-08T09:05:45Z creation;2022-06-08T09:05:46Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220608090545  20220610141505  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               yA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��^�^i1   @��_J�A�@/޸Q��c{t�j~�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP��BW33B_��Bh  Bp  Bx  B�  B���B���B�  B�ffB�  B�  B�  B�  B�  B���B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  C   C�fC�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(�C)�fC,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ�fD[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ Dݼ�D���D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @z�@z�H@�p�@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BHzBPz�BV�GB_G�Bg�Bo�Bw�B�B���B�p�B��
B�=pB��
B��
B��
B��
B��
B�p�B���B��
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
B�
=B�
=B��
B��
B��
B��
B��
C��C��C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C&C(C)��C+�C-�C/�C1�C3�C5��C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CVCW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DZGDZ�GDZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��>D��qD�=qD�}qD��>D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD��qD�=qD�}qDݺ>D��>D�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD〤D���D��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��>D�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�g1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�uZA�w2A�|A�x�A�x8A�w�A�}"A�z�A�{�A�|�A�}�À4Á;ÁoÁ;A�}VA�u�A�^A�K�A�9$A�33A�.IA�+�A�(�A�&�A�#�A��A˥A�ZA�uAʣ�A�t�Aɥ�A�	A��0Aƭ�A��A�{�A�˒A�kA��	A���A��A��A���A�O�A�m�A�#:A�A��AA�Q�A��TA�ѷA�e�A��TA��
A�;�A��BA�LdA�b�A�,�A�q�A��0A���A���A�OA�d�A�NpA��+A�%�A�A�A��aA��xA�˒A�YA���A��cA���A�0�A��4A��A��A�FA�FA|p;Av��Ar7LAqjAmX�Ag�	Aa��A]C-AZ�kAW��AU�9AT�CAR iAN]dAL�?AJt�AG�AAE�ADĜAC}VAB&�A?F�A=��A<�A<>BA;TaA:�XA8 \A7��A7G�A6b�A4�fA4r�A3�ZA2��A2�A1A0O�A/\)A-�>A-B�A,�sA+7�A*/A)� A)1�A(�.A(�6A(��A'�TA'l�A'�DA(1'A(&�A'cA&یA&��A&��A&=qA%xlA%�A#��A"�WA"�$A"kQA"A!�2A!�jA!�A ��A L�A�pA�A,=A�A�zAjA�PA�AHA6zA��A��AW�AƨAD�A�QA��A4AȴA�DA;�A��A�AȴAY�A�`Aa|A�A�&As�AJAL0A��A=A�A�A\�A	A��A?�A�AϫAN<A�A��A�A��A� AoiA\)ADgA
��A	�hA	y>A	_AK^A	lA��A>�A�A��A��Aa|A�aA��A�	A6zA�$A�nA�YAZAFtA��A�0A��A�A� A ��A ?�@���@���@�z@�{@��h@��@���@���@�J�@�S@��r@��V@�ff@���@���@�0U@��#@��@��@�P�@���@�xl@�]d@�=q@�+k@���@��@�(�@�@�qv@�A�@��@��@�+@�5�@�V@�I@�4n@�b@��@�]d@��@��@�	@�J#@�u%@�*0@�r�@��@���@�ݘ@�u�@�C-@���@�0@�e�@�.I@���@�@��M@�q@�^�@���@�V@ޟ�@�h
@��@ݢ�@�`B@�5�@���@�+�@ڍ�@ٚk@�-w@��@�+@�[W@��K@��@ה�@֧�@ժ�@�_p@�@O@��@�Z�@ӌ~@��2@�m�@��>@є�@�&@Ь�@��9@Ϧ�@��`@�&�@�O�@���@ζ�@Ρb@·�@�!�@��[@̃@���@�2a@ʷ�@�*�@ɼ@�Y�@Ⱥ�@��}@�Ĝ@��m@ŀ4@�$t@��[@ă@�Z@���@�+�@¬@�a|@��@���@�/@���@�V@�!@��z@�c@�<6@��h@�0U@���@��@�4�@���@���@�Z@��@��S@�[W@���@��U@�~(@�Z�@��@���@�?}@��@�:�@��Z@�7L@��2@���@�c�@�  @�ƨ@�N<@��h@�~@�W?@��@��@�^5@�1@���@��Q@��'@�Z�@�{�@�u@��S@�1�@��@��o@�d�@��@��H@�=@��/@��.@�u�@�3�@�x@��+@��j@���@���@��@��@�g�@���@�kQ@�b@�خ@��z@��*@��@�ی@���@�?@��}@��w@���@���@�M�@���@�<6@��o@��@��V@�j�@��@�r�@��C@���@�p�@��@�ȴ@��h@�@��o@��@�J@���@�<�@���@�O@�<6@��9@��@���@�@@��@��@��f@���@�?@�$@��@��
@���@�|�@�W?@�>�@�4@��@��@���@�*�@��@��@@�<6@��@���@��@���@���@���@�hs@�O�@��@�L0@�($@� �@�7@��@��;@�a�@���@�E�@��@��;@�/@��U@�$@���@��P@�Dg@�0�@�&�@�%F@�&�@�@��U@�r�@��@��Q@�� @���@��"@�_p@��@��@��@��B@�~(@�	@�ϫ@���@�p�@�c�@�B�@�	l@���@��P@��@�tT@�A�@��@���@���@�~�@�4@��@��H@��4@�D�@�<�@��@�@���@�Z�@�E9@�5�@�*0@��@�1'@�ݘ@���@��7@�\�@�1�@�q@���@�͟@��@�7@��.@��a@��	@�W?@�	l@��@��@��$@��+@�5?@��6@��C@�s@�+@���@��}@���@�Q�@�,=@��@���@���@�<6@�4�@�,�@��@��2@�H@�w@��@�:@��@�4@y�@g�@~��@~1�@}ԕ@}k�@}L�@}!�@|��@|PH@{�F@{/�@z�!@zv�@y�>@y�@y[W@y7L@y%F@x�@x�@w��@wa@wJ#@w1�@w�@v�L@u��@u\�@t�@t_@t  @s�@r}V@rh
@rn�@rL0@q�"@p�p@o�@og�@n��@nH�@m�z@m�@l�z@lXy@k�q@k�@j��@jM�@j!�@j�@j!�@j!�@i�j@i�S@iu�@i�@hg8@g�;@g��@g��@gS�@gC@f�@fz@e�@eL�@e(�@d��@d�@dl"@d_@d*�@d:�@c�@b�c@b.�@b)�@b@b�@b�@a�j@a�~@a8�@`PH@_�g@_4�@_�@^�'@^�+@^e@]�@]��@]=�@\�4@\e�@\U2@\@\G@[�K@[_p@[�@ZC�@X��@X`�@XC-@X!@W��@W˒@W��@W'�@W�@V��@Vxl@VOv@VB[@VGE@V;�@U��@U��@U+@T�@T��@TtT@TG@S��@R��@R��@R	@Q�M@PɆ@PD�@O�]@O�*@O=@N�X@N�@MrG@M;@L9X@K�@J�s@Ja|@J=q@J�@IG�@H��@H�@Hw�@Hj@HN�@H9X@G�W@G��@G�@G@O@F@Em]@Dg8@CO@B��@B��@B��@B��@Bn�@BTa@B�@Ax�@A�@A@@A�@A�@A�@@��@@�E@@��@@G@?��@?e�@>�@>YK@>8�@=�C@=J�@<��@<��@<��@<�@<e�@<!@;�W@;��@;@:�@9��@9s�@8�O@8�@8�@7ݘ@7v`@7C�@7+@7S@6�@6��@6�m@6��@6~�@6YK@6:*@6:*@61�@6e@5^�@4��@4�E@4�@4�U@4�@4�o@4]d@4N�@4�@3�[@3��@3!-@2��@2��@28�@1��@1}�@0�@0�e@0��@0�@0q@0`�@/ݘ@/�@/n/@/(@.�,@.��@.l�@-�@-�7@-<6@-&�@,�@,��@,��@,�o@,Q�@,�@+�&@+�6@+��@+��@+��@+�*@+��@+x@+]�@+6z@*��@*��@*� @*z@*Q@)��@)��@)��@)hs@)=�@(�v@(��@(��@(r�@(c�@(S�@('R@(�@(�@'��@'O@&��@&&�@%�C@%e,@%Q�@%8�@$�5@$�e@$�4@$�_@$h�@$9X@#�@#�F@#�:@#t�@#X�@#&@"�c@"�m@"��@"i�@"GE@")�@"4@!�T@!�@!rG@ ��@ q@ PH@ �@��@��@u%@d�@J�@$�@	@u@��@`B@q@�`@�I@��@��@u�@Xy@A�@'R@�+@��@$t@ں@��@��@��@��@��@~�@J�@��@�@��@��@��@Dg@+@�[@��@Q�@�m@o�@9�@'�@ i@��@�,@��@Q@��@�N@�@�H@��@�@��@�@5�@��@��@�@�.@m�@4n@x@�@�@�@|�@C�@ȴ@��@~�@z@l�@6�@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�uZA�w2A�|A�x�A�x8A�w�A�}"A�z�A�{�A�|�A�}�À4Á;ÁoÁ;A�}VA�u�A�^A�K�A�9$A�33A�.IA�+�A�(�A�&�A�#�A��A˥A�ZA�uAʣ�A�t�Aɥ�A�	A��0Aƭ�A��A�{�A�˒A�kA��	A���A��A��A���A�O�A�m�A�#:A�A��AA�Q�A��TA�ѷA�e�A��TA��
A�;�A��BA�LdA�b�A�,�A�q�A��0A���A���A�OA�d�A�NpA��+A�%�A�A�A��aA��xA�˒A�YA���A��cA���A�0�A��4A��A��A�FA�FA|p;Av��Ar7LAqjAmX�Ag�	Aa��A]C-AZ�kAW��AU�9AT�CAR iAN]dAL�?AJt�AG�AAE�ADĜAC}VAB&�A?F�A=��A<�A<>BA;TaA:�XA8 \A7��A7G�A6b�A4�fA4r�A3�ZA2��A2�A1A0O�A/\)A-�>A-B�A,�sA+7�A*/A)� A)1�A(�.A(�6A(��A'�TA'l�A'�DA(1'A(&�A'cA&یA&��A&��A&=qA%xlA%�A#��A"�WA"�$A"kQA"A!�2A!�jA!�A ��A L�A�pA�A,=A�A�zAjA�PA�AHA6zA��A��AW�AƨAD�A�QA��A4AȴA�DA;�A��A�AȴAY�A�`Aa|A�A�&As�AJAL0A��A=A�A�A\�A	A��A?�A�AϫAN<A�A��A�A��A� AoiA\)ADgA
��A	�hA	y>A	_AK^A	lA��A>�A�A��A��Aa|A�aA��A�	A6zA�$A�nA�YAZAFtA��A�0A��A�A� A ��A ?�@���@���@�z@�{@��h@��@���@���@�J�@�S@��r@��V@�ff@���@���@�0U@��#@��@��@�P�@���@�xl@�]d@�=q@�+k@���@��@�(�@�@�qv@�A�@��@��@�+@�5�@�V@�I@�4n@�b@��@�]d@��@��@�	@�J#@�u%@�*0@�r�@��@���@�ݘ@�u�@�C-@���@�0@�e�@�.I@���@�@��M@�q@�^�@���@�V@ޟ�@�h
@��@ݢ�@�`B@�5�@���@�+�@ڍ�@ٚk@�-w@��@�+@�[W@��K@��@ה�@֧�@ժ�@�_p@�@O@��@�Z�@ӌ~@��2@�m�@��>@є�@�&@Ь�@��9@Ϧ�@��`@�&�@�O�@���@ζ�@Ρb@·�@�!�@��[@̃@���@�2a@ʷ�@�*�@ɼ@�Y�@Ⱥ�@��}@�Ĝ@��m@ŀ4@�$t@��[@ă@�Z@���@�+�@¬@�a|@��@���@�/@���@�V@�!@��z@�c@�<6@��h@�0U@���@��@�4�@���@���@�Z@��@��S@�[W@���@��U@�~(@�Z�@��@���@�?}@��@�:�@��Z@�7L@��2@���@�c�@�  @�ƨ@�N<@��h@�~@�W?@��@��@�^5@�1@���@��Q@��'@�Z�@�{�@�u@��S@�1�@��@��o@�d�@��@��H@�=@��/@��.@�u�@�3�@�x@��+@��j@���@���@��@��@�g�@���@�kQ@�b@�خ@��z@��*@��@�ی@���@�?@��}@��w@���@���@�M�@���@�<6@��o@��@��V@�j�@��@�r�@��C@���@�p�@��@�ȴ@��h@�@��o@��@�J@���@�<�@���@�O@�<6@��9@��@���@�@@��@��@��f@���@�?@�$@��@��
@���@�|�@�W?@�>�@�4@��@��@���@�*�@��@��@@�<6@��@���@��@���@���@���@�hs@�O�@��@�L0@�($@� �@�7@��@��;@�a�@���@�E�@��@��;@�/@��U@�$@���@��P@�Dg@�0�@�&�@�%F@�&�@�@��U@�r�@��@��Q@�� @���@��"@�_p@��@��@��@��B@�~(@�	@�ϫ@���@�p�@�c�@�B�@�	l@���@��P@��@�tT@�A�@��@���@���@�~�@�4@��@��H@��4@�D�@�<�@��@�@���@�Z�@�E9@�5�@�*0@��@�1'@�ݘ@���@��7@�\�@�1�@�q@���@�͟@��@�7@��.@��a@��	@�W?@�	l@��@��@��$@��+@�5?@��6@��C@�s@�+@���@��}@���@�Q�@�,=@��@���@���@�<6@�4�@�,�@��@��2@�H@�w@��@�:@��@�4@y�@g�@~��@~1�@}ԕ@}k�@}L�@}!�@|��@|PH@{�F@{/�@z�!@zv�@y�>@y�@y[W@y7L@y%F@x�@x�@w��@wa@wJ#@w1�@w�@v�L@u��@u\�@t�@t_@t  @s�@r}V@rh
@rn�@rL0@q�"@p�p@o�@og�@n��@nH�@m�z@m�@l�z@lXy@k�q@k�@j��@jM�@j!�@j�@j!�@j!�@i�j@i�S@iu�@i�@hg8@g�;@g��@g��@gS�@gC@f�@fz@e�@eL�@e(�@d��@d�@dl"@d_@d*�@d:�@c�@b�c@b.�@b)�@b@b�@b�@a�j@a�~@a8�@`PH@_�g@_4�@_�@^�'@^�+@^e@]�@]��@]=�@\�4@\e�@\U2@\@\G@[�K@[_p@[�@ZC�@X��@X`�@XC-@X!@W��@W˒@W��@W'�@W�@V��@Vxl@VOv@VB[@VGE@V;�@U��@U��@U+@T�@T��@TtT@TG@S��@R��@R��@R	@Q�M@PɆ@PD�@O�]@O�*@O=@N�X@N�@MrG@M;@L9X@K�@J�s@Ja|@J=q@J�@IG�@H��@H�@Hw�@Hj@HN�@H9X@G�W@G��@G�@G@O@F@Em]@Dg8@CO@B��@B��@B��@B��@Bn�@BTa@B�@Ax�@A�@A@@A�@A�@A�@@��@@�E@@��@@G@?��@?e�@>�@>YK@>8�@=�C@=J�@<��@<��@<��@<�@<e�@<!@;�W@;��@;@:�@9��@9s�@8�O@8�@8�@7ݘ@7v`@7C�@7+@7S@6�@6��@6�m@6��@6~�@6YK@6:*@6:*@61�@6e@5^�@4��@4�E@4�@4�U@4�@4�o@4]d@4N�@4�@3�[@3��@3!-@2��@2��@28�@1��@1}�@0�@0�e@0��@0�@0q@0`�@/ݘ@/�@/n/@/(@.�,@.��@.l�@-�@-�7@-<6@-&�@,�@,��@,��@,�o@,Q�@,�@+�&@+�6@+��@+��@+��@+�*@+��@+x@+]�@+6z@*��@*��@*� @*z@*Q@)��@)��@)��@)hs@)=�@(�v@(��@(��@(r�@(c�@(S�@('R@(�@(�@'��@'O@&��@&&�@%�C@%e,@%Q�@%8�@$�5@$�e@$�4@$�_@$h�@$9X@#�@#�F@#�:@#t�@#X�@#&@"�c@"�m@"��@"i�@"GE@")�@"4@!�T@!�@!rG@ ��@ q@ PH@ �@��@��@u%@d�@J�@$�@	@u@��@`B@q@�`@�I@��@��@u�@Xy@A�@'R@�+@��@$t@ں@��@��@��@��@��@~�@J�@��@�@��@��@��@Dg@+@�[@��@Q�@�m@o�@9�@'�@ i@��@�,@��@Q@��@�N@�@�H@��@�@��@�@5�@��@��@�@�.@m�@4n@x@�@�@�@|�@C�@ȴ@��@~�@z@l�@6�@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	vFB	v`B	v`B	vzB	vzB	w2B	v�B	y>B	y$B	x�B	z^B	z�B	z�B	{JB	{�B	}"B	� B	��B	�B	��B	��B	��B	�FB	�B	�+B	�QB	��B	ƨB	�B	��B
�B
,"B
fB@B/OB7fB+BR�BSuBS@Bf�Bg�Bf�BR�B7�B,"B�B�B[B�B{B�B�B��B��B	BbB	�B_B��B�B�%B�B��B�zB��B��B�B�~B��Bw�Ba�BJ�B8�B"BB
�DB
�B
ʌB
�B
u�B
<�B
�B	��B	�>B	��B	�^B	�ZB	��B	��B	iDB	S�B	HKB	:�B	2-B	+�B	'B	!�B	�B	!�B	$@B	*�B	-�B	/ B	2�B	1B	4B	=�B	?cB	=�B	B'B	H�B	E�B	C�B	A�B	O(B	VmB	X�B	W�B	T�B	P�B	V9B	_VB	[�B	\�B	`'B	fB	kkB	vB	��B	��B	��B	��B	�XB	��B	��B	�9B	�B	�B	ٚB	�5B	��B	��B	�B	�hB	��B	�B	�+B	ںB	�B	��B	�B	�=B	�B	��B	��B
B
9B
_B

�B
�B

XB
�B
�B
BB
VB
"B
�B
�B
B
(B
�B
�B
�B
KB
eB
EB
�B
@B
�B
bB
�B
B
�B
2B
:B
B
�B
B
�B
�B
aB
uB
B
�B
sB
?B
�B
�B
�B
�B
YB
�B

B
�B
�B
�B
B
yB
YB
B
B
�B
B
B
�B
�B
 B

�B
�B
VB
6B
�B
�B
pB
jB
}B
B
BB
�B
EB
�B
�B
AB
�B
�B
�B
AB
�B
[B
�B
;B
 B
 �B
 iB	��B	�B	��B	�fB	��B	��B	�B	�ZB	�TB	�TB	�B	�B	�tB	�`B	��B	��B	��B	�2B	��B	�B	��B	��B	��B
 OB
B
 �B	��B
 iB
�B
 B
oB
;B
 B
 �B
 �B
 4B	��B	�.B	��B	��B	��B	�qB	��B	��B	��B	�jB	�jB	�B	�B	�XB	�	B	�RB	�B	��B	��B	��B	��B	��B	��B	�2B	��B	�TB	�9B	�%B	��B	�`B	�XB	��B	��B	�lB	��B	��B	�rB	��B	�XB	��B	�fB	��B	�tB	��B	�B	�B	�|B	�B	��B	�nB	�2B	�B	��B	��B	��B	�B	�]B	��B	�dB	�DB	�$B	��B	�B	��B	�LB	��B	��B	��B	��B	�2B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�JB	�PB	��B	��B	��B	�B	�B	�wB	�B	��B	��B
 �B	�}B	��B	��B	�B	��B	��B	��B	�6B	�PB	�6B	��B	��B	�VB	�VB	�"B	�qB	��B	��B	�VB	��B	��B	��B	��B	�B	��B	�]B	�wB	�.B	�HB	�cB	�cB	�cB	�}B	��B	��B	��B	��B	��B
 4B
 �B
 �B
B
�B
�B
GB
-B
B
�B
GB
�B
�B
[B
uB
�B
B
�B
'B
�B
aB
GB
GB
�B
�B
[B
'B
B
�B
�B
UB
UB
B
 B	�.B	��B	��B	��B	�DB	��B	��B	��B	�`B	�FB	��B	��B	��B	��B	��B	��B
 �B
�B	��B	��B	�6B	�dB	��B
�B
tB
+B
�B
�B
fB
xB
jB
B
"B
B
BB
B
�B
 B
B
B
�B
�B
@B
�B
gB
gB
�B
�B
�B

B
yB
+B
_B
+B
B
�B
B
B
B
B
B
�B
�B
�B
#B
�B
�B
�B
B
B
/B
B
OB
�B
B
B
!B
!B
�B
 'B
 �B
 �B
 �B
!|B
!|B
!�B
"B
"4B
"B
"B
"hB
"�B
"�B
# B
#�B
#nB
#�B
#�B
#�B
#�B
#nB
#�B
#�B
$B
$&B
$&B
$�B
%FB
%FB
%�B
%�B
%�B
%`B
%�B
%zB
&2B
&LB
%�B
&fB
&�B
&�B
'�B
'8B
'8B
'RB
'B
&�B
&�B
'8B
'8B
'�B
(
B
'�B
(
B
'�B
(
B
($B
($B
(>B
(�B
)B
)_B
)DB
)yB
)�B
*0B
*�B
*�B
*�B
+6B
+�B
,�B
,�B
-)B
-�B
-wB
-�B
-�B
-�B
.cB
.�B
.�B
.�B
.�B
.�B
.�B
/B
0!B
0�B
1B
1vB
1�B
1�B
2�B
2�B
3�B
49B
4�B
4�B
5%B
5?B
5�B
5�B
5tB
5�B
6FB
6�B
6�B
6�B
6�B
7B
7�B
8B
8�B
9$B
9$B
9$B
8�B
8�B
9	B
9�B
:*B
:^B
:�B
;JB
;dB
:�B
:^B
:DB
:�B
;0B
;�B
<B
;�B
;B
<PB
<jB
<�B
<�B
<jB
="B
=VB
=qB
=�B
>�B
>�B
>�B
>�B
?}B
?}B
?�B
?�B
@ B
@�B
@�B
@�B
@�B
@�B
@�B
A;B
BuB
CaB
C�B
C{B
C�B
D3B
D�B
D�B
E�B
E�B
E�B
D�B
C�B
CB
B�B
B�B
CGB
DB
DgB
D�B
E�B
F�B
F�B
GEB
G�B
H1B
H�B
H�B
HB
I7B
H�B
HB
HB
H1B
H�B
HfB
H�B
H�B
H�B
I7B
I�B
JXB
J�B
K)B
MB
L�B
NVB
N�B
O(B
O�B
O�B
O�B
O�B
PB
PbB
P�B
Q B
Q�B
QhB
Q�B
Q�B
QNB
Q�B
R�B
SB
S[B
S�B
TB
T�B
T�B
T�B
T�B
UgB
U�B
U�B
U�B
U�B
VB
U�B
V9B
V9B
VB
VmB
W?B
WsB
XB
X�B
X�B
X�B
YB
Y1B
YKB
YKB
Y�B
ZQB
ZkB
Z�B
ZkB
Z�B
ZkB
Z�B
Z�B
Z�B
[#B
[qB
[WB
[�B
\B
[�B
[�B
\B
\xB
\xB
\xB
\�B
\xB
\�B
\�B
\�B
]dB
]�B
]�B
^5B
_!B
_�B
_VB
_�B
_�B
`B
`'B
`�B
aB
a|B
a�B
a�B
a�B
a�B
bB
a�B
a�B
a�B
b�B
c B
cB
c B
c B
c B
cTB
cTB
c:B
c:B
b�B
c B
cB
b�B
b�B
b�B
b�B
c B
c�B
c�B
c�B
c�B
c�B
c�B
dtB
dZB
d�B
d�B
d�B
d�B
e,B
e`B
e�B
e�B
e�B
fB
fLB
ffB
fLB
f�B
f�B
f�B
f�B
f�B
gB
gB
f�B
f�B
gB
gB
g8B
g�B
g�B
g�B
g�B
g�B
hXB
hsB
h�B
h�B
h�B
iDB
iyB
iyB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jeB
j�B
k�B
k�B
k�B
lB
k�B
lqB
l�B
l�B
l�B
l�B
l�B
mCB
mwB
m�B
m�B
m�B
m�B
nB
nB
nIB
ncB
n�B
n�B
n�B
n�B
n�B
o5B
o�B
o�B
o�B
p;B
pUB
qvB
q�B
q�B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
sMB
sMB
sMB
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u%B
utB
u�B
u�B
u�B
u�B
v+B
v+B
vzB
v`B
v�B
wLB
w�B
w�B
w�B
x8B
xB
x8B
x�B
x�B
y$B
y>B
y>B
yXB
y>B
y>B
yXB
yrB
y�B
z*B
z^B
zDB
zxB
z�B
z�B
z�B
z�B
z�B
{JB
{0B
{�B
|6B
|PB
|jB
|jB
|jB
|�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	vFB	v`B	v`B	vzB	vzB	w2B	v�B	y>B	y$B	x�B	z^B	z�B	z�B	{JB	{�B	}"B	� B	��B	�B	��B	��B	��B	�FB	�B	�+B	�QB	��B	ƨB	�B	��B
�B
,"B
fB@B/OB7fB+BR�BSuBS@Bf�Bg�Bf�BR�B7�B,"B�B�B[B�B{B�B�B��B��B	BbB	�B_B��B�B�%B�B��B�zB��B��B�B�~B��Bw�Ba�BJ�B8�B"BB
�DB
�B
ʌB
�B
u�B
<�B
�B	��B	�>B	��B	�^B	�ZB	��B	��B	iDB	S�B	HKB	:�B	2-B	+�B	'B	!�B	�B	!�B	$@B	*�B	-�B	/ B	2�B	1B	4B	=�B	?cB	=�B	B'B	H�B	E�B	C�B	A�B	O(B	VmB	X�B	W�B	T�B	P�B	V9B	_VB	[�B	\�B	`'B	fB	kkB	vB	��B	��B	��B	��B	�XB	��B	��B	�9B	�B	�B	ٚB	�5B	��B	��B	�B	�hB	��B	�B	�+B	ںB	�B	��B	�B	�=B	�B	��B	��B
B
9B
_B

�B
�B

XB
�B
�B
BB
VB
"B
�B
�B
B
(B
�B
�B
�B
KB
eB
EB
�B
@B
�B
bB
�B
B
�B
2B
:B
B
�B
B
�B
�B
aB
uB
B
�B
sB
?B
�B
�B
�B
�B
YB
�B

B
�B
�B
�B
B
yB
YB
B
B
�B
B
B
�B
�B
 B

�B
�B
VB
6B
�B
�B
pB
jB
}B
B
BB
�B
EB
�B
�B
AB
�B
�B
�B
AB
�B
[B
�B
;B
 B
 �B
 iB	��B	�B	��B	�fB	��B	��B	�B	�ZB	�TB	�TB	�B	�B	�tB	�`B	��B	��B	��B	�2B	��B	�B	��B	��B	��B
 OB
B
 �B	��B
 iB
�B
 B
oB
;B
 B
 �B
 �B
 4B	��B	�.B	��B	��B	��B	�qB	��B	��B	��B	�jB	�jB	�B	�B	�XB	�	B	�RB	�B	��B	��B	��B	��B	��B	��B	�2B	��B	�TB	�9B	�%B	��B	�`B	�XB	��B	��B	�lB	��B	��B	�rB	��B	�XB	��B	�fB	��B	�tB	��B	�B	�B	�|B	�B	��B	�nB	�2B	�B	��B	��B	��B	�B	�]B	��B	�dB	�DB	�$B	��B	�B	��B	�LB	��B	��B	��B	��B	�2B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�JB	�PB	��B	��B	��B	�B	�B	�wB	�B	��B	��B
 �B	�}B	��B	��B	�B	��B	��B	��B	�6B	�PB	�6B	��B	��B	�VB	�VB	�"B	�qB	��B	��B	�VB	��B	��B	��B	��B	�B	��B	�]B	�wB	�.B	�HB	�cB	�cB	�cB	�}B	��B	��B	��B	��B	��B
 4B
 �B
 �B
B
�B
�B
GB
-B
B
�B
GB
�B
�B
[B
uB
�B
B
�B
'B
�B
aB
GB
GB
�B
�B
[B
'B
B
�B
�B
UB
UB
B
 B	�.B	��B	��B	��B	�DB	��B	��B	��B	�`B	�FB	��B	��B	��B	��B	��B	��B
 �B
�B	��B	��B	�6B	�dB	��B
�B
tB
+B
�B
�B
fB
xB
jB
B
"B
B
BB
B
�B
 B
B
B
�B
�B
@B
�B
gB
gB
�B
�B
�B

B
yB
+B
_B
+B
B
�B
B
B
B
B
B
�B
�B
�B
#B
�B
�B
�B
B
B
/B
B
OB
�B
B
B
!B
!B
�B
 'B
 �B
 �B
 �B
!|B
!|B
!�B
"B
"4B
"B
"B
"hB
"�B
"�B
# B
#�B
#nB
#�B
#�B
#�B
#�B
#nB
#�B
#�B
$B
$&B
$&B
$�B
%FB
%FB
%�B
%�B
%�B
%`B
%�B
%zB
&2B
&LB
%�B
&fB
&�B
&�B
'�B
'8B
'8B
'RB
'B
&�B
&�B
'8B
'8B
'�B
(
B
'�B
(
B
'�B
(
B
($B
($B
(>B
(�B
)B
)_B
)DB
)yB
)�B
*0B
*�B
*�B
*�B
+6B
+�B
,�B
,�B
-)B
-�B
-wB
-�B
-�B
-�B
.cB
.�B
.�B
.�B
.�B
.�B
.�B
/B
0!B
0�B
1B
1vB
1�B
1�B
2�B
2�B
3�B
49B
4�B
4�B
5%B
5?B
5�B
5�B
5tB
5�B
6FB
6�B
6�B
6�B
6�B
7B
7�B
8B
8�B
9$B
9$B
9$B
8�B
8�B
9	B
9�B
:*B
:^B
:�B
;JB
;dB
:�B
:^B
:DB
:�B
;0B
;�B
<B
;�B
;B
<PB
<jB
<�B
<�B
<jB
="B
=VB
=qB
=�B
>�B
>�B
>�B
>�B
?}B
?}B
?�B
?�B
@ B
@�B
@�B
@�B
@�B
@�B
@�B
A;B
BuB
CaB
C�B
C{B
C�B
D3B
D�B
D�B
E�B
E�B
E�B
D�B
C�B
CB
B�B
B�B
CGB
DB
DgB
D�B
E�B
F�B
F�B
GEB
G�B
H1B
H�B
H�B
HB
I7B
H�B
HB
HB
H1B
H�B
HfB
H�B
H�B
H�B
I7B
I�B
JXB
J�B
K)B
MB
L�B
NVB
N�B
O(B
O�B
O�B
O�B
O�B
PB
PbB
P�B
Q B
Q�B
QhB
Q�B
Q�B
QNB
Q�B
R�B
SB
S[B
S�B
TB
T�B
T�B
T�B
T�B
UgB
U�B
U�B
U�B
U�B
VB
U�B
V9B
V9B
VB
VmB
W?B
WsB
XB
X�B
X�B
X�B
YB
Y1B
YKB
YKB
Y�B
ZQB
ZkB
Z�B
ZkB
Z�B
ZkB
Z�B
Z�B
Z�B
[#B
[qB
[WB
[�B
\B
[�B
[�B
\B
\xB
\xB
\xB
\�B
\xB
\�B
\�B
\�B
]dB
]�B
]�B
^5B
_!B
_�B
_VB
_�B
_�B
`B
`'B
`�B
aB
a|B
a�B
a�B
a�B
a�B
bB
a�B
a�B
a�B
b�B
c B
cB
c B
c B
c B
cTB
cTB
c:B
c:B
b�B
c B
cB
b�B
b�B
b�B
b�B
c B
c�B
c�B
c�B
c�B
c�B
c�B
dtB
dZB
d�B
d�B
d�B
d�B
e,B
e`B
e�B
e�B
e�B
fB
fLB
ffB
fLB
f�B
f�B
f�B
f�B
f�B
gB
gB
f�B
f�B
gB
gB
g8B
g�B
g�B
g�B
g�B
g�B
hXB
hsB
h�B
h�B
h�B
iDB
iyB
iyB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jeB
j�B
k�B
k�B
k�B
lB
k�B
lqB
l�B
l�B
l�B
l�B
l�B
mCB
mwB
m�B
m�B
m�B
m�B
nB
nB
nIB
ncB
n�B
n�B
n�B
n�B
n�B
o5B
o�B
o�B
o�B
p;B
pUB
qvB
q�B
q�B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
sMB
sMB
sMB
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u%B
utB
u�B
u�B
u�B
u�B
v+B
v+B
vzB
v`B
v�B
wLB
w�B
w�B
w�B
x8B
xB
x8B
x�B
x�B
y$B
y>B
y>B
yXB
y>B
y>B
yXB
yrB
y�B
z*B
z^B
zDB
zxB
z�B
z�B
z�B
z�B
z�B
{JB
{0B
{�B
|6B
|PB
|jB
|jB
|jB
|�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220608090157  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220608090545  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220608090546  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220608090546                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220608180551  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220608180551  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                