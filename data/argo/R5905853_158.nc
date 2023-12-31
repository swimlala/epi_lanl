CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-06-08T12:47:49Z creation;2023-06-08T12:47:50Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230608124749  20230608125710  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�1YЗ�&1   @�1Z?%��@0���l�D�c�����1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@���@���AffA@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B7��B@  BH  BPffBX  B`  Bh  Bq33Bx  B��B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B���B���B���B�  BЙ�Bә�B���B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C33C��C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(33C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DB��DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz�fD{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�3D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@�=q@�=qA�A>�RA^�RA~�RA�\)A�\)A�\)A��\A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7G�B?�BG�BPzBW�B_�Bg�Bp�GBw�BG�B��
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
B���B���Bã�Bǣ�B��
B�p�B�p�Bף�B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
C�C�C�C�RC	�C�C��C�C�C�C�C�C�C�C�C�C!�C#�C%�C(�C)��C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C`CbCc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy��C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D%GD%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB�{DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dz�GDz��D{z�D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD� �D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD½qD��qD�=qD�}qDýqD��qD�=qD�}qDĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��qD�=qD�}qDǽqD��qD�=qD�}qDȽqD��qD�=qD�}qDɽqD��qD�=qD�}qDʽqD��qD�=qD�}qD˽qD��qD�=qD�}qD̽qD��qD�=qD�}qDͽqD��qD�=qD�}qDνqD��qD�=qD�}qDϽqD��qD�=qD�}qDнqD��qD�=qD�}qDѽqD��qD�=qD�}qDҽqD��qD�=qD�}qDӽqD��qD�=qD�}qDԽqD��qD�=qD�}qDսqD��qD�=qD�}qDֽqD��qD�=qD�}qD׽qD��qD�=qD�}qDؽqD��qD�=qD�}qDٽqD��qD�=qD�}qDڽqD��qD�=qD�}qD۽qD��qD�=qD�}qDܽqD� �D�=qD�}qDݽqD��qD�=qD�}qD޽qD��qD�=qD�}qD߽qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD�qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD���D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�ŢA��}A���A�ޞA��A���A���A��dA���A���A��sA���A�یA��)A�ܒA��QA��5A���A��sA��aA��pA��aĄXA̞A̚kA̘�A̜ẠnA̅�A�K^A˿}A˛�A�e�A�#:A�S&A�QA�"hA��aA�_�A��A�b�A�ݘA���A��FA��qA��.A�8�A�ٴA��A���A���A��AA�O�A��TA�c�A�uA�g�A�r�A��A�=qA�T,A��~A�ߤA�ҽA��A��SA���A���A�S[A�v�A���A�S&A�\)A��A�
�A~(A|�+AwR�AtC�As�Aq1'An��Al<�Af��Ac�0A^Q�A]�AV�`AR�ANZAK?�AF�:ABYA?��A=bA<�A:�A7dZA55?A3�#A2˒A2��A1XA0/�A.S&A,�A*�A)�}A(�A()�A'�eA'(�A&jA%�pA$�;A#�A"*0A ��As�Ax�A�A�jA \A$AAe�A�A�dA8�AȴA��A��A,�A�A�A��A�A�hA�&A� AYA�fA��A��A�AU�AGA	��A��A�
A�EAb�A��A�DA�tA�#Ap;A�A ��@���@��t@���@��w@�M@��$@�n/@�s�@�1'@�s@���@��'@�� @�u%@��@��A s�A2aA�A��A ��A �aA +@��@��>@�a�@��@���@�i�@��@�2�@��=@�8@�?@�\�@��r@�zx@���@��@�,�@�S�@�q@�g�@@�Xy@��@���@�7�@�e@�{@�`B@�o@���@��@�@�,�@��D@�`�@���@�33@�qv@�C@�O�@��M@��@�	@�u�@��
@�a@��@��2@�`�@ߓ@�\�@�,�@޾@�?@�6�@��@�H�@�$t@�9�@���@��@�u%@�e@ۭC@��f@�_�@٢�@�S&@���@�z�@�"h@�Q�@��@�Xy@�4@ՠ�@�+�@��@ԝI@��Q@�j@�2a@�_@Ѻ^@�4@�h
@���@�dZ@�*0@̧�@�c�@̌�@��@�e@ˇ�@��@ʞ�@��@�˒@Ɏ"@��@��@ș1@�-�@�ƨ@��@ƌ@��@�A�@��@č�@�(�@Ò:@��@�ѷ@«6@�R�@���@��f@�-@���@��$@�Y�@�@O@�!�@��@�:*@��@�/�@��6@�C-@��}@�E9@���@��.@�R�@�Ov@��+@�s�@�/@�	l@��y@���@�W�@�+k@��@���@�C�@�@��I@�`�@��@�e,@�2a@�c�@�
�@���@��7@�E9@��@�� @�,=@��=@�!�@��@�W�@��;@��7@���@�z�@�ԕ@��@���@���@�>B@��a@���@��@�c @���@�@��?@�C�@���@��[@�(@�ff@���@�_@��j@��@�,�@���@�[�@�@�@���@��}@���@�-w@�J#@���@��!@��A@�Ov@��@���@���@�RT@��j@���@�.�@���@�A @��R@�Q�@�  @��@�U�@�S&@�6z@�+@�+@��@��O@���@�m�@�c @�9X@���@�~�@�v`@��	@�S�@�K�@�33@��@��[@�4n@��@��*@�]�@�;@���@�6@��F@�\)@��@���@�p;@�u@���@���@��@���@�zx@�F@��@�e@���@�s@�4�@��@�֡@���@�bN@���@�F�@��@���@��@�U2@�%�@��@�b@���@��@���@��@���@��b@��D@�a|@��@���@��@�A @��y@���@��@���@�t�@��@���@�3�@��:@�F�@��@�(�@�ȴ@�n�@�@��"@��M@��@���@���@��A@�_@�!@���@�qv@�[W@�J#@��@��}@�d�@��@�e,@�=�@�"�@���@���@�3�@���@��X@���@�G�@��|@��9@��x@��.@��@�M@��@���@�?}@��@���@�֡@��D@�R�@�+k@��@��P@�k�@�[W@�S�@�RT@�P�@�5�@�Ɇ@��@��A@�r�@�W�@�*�@�+@��@�$@,�@S@~��@}��@}[W@}%@|�v@|��@|��@}�@}��@}x�@|�e@|>B@{�@@{"�@z�}@z��@zxl@yu�@x�@x��@xoi@xg8@xXy@x$@w��@w�@v�@v�B@v��@v�F@uϫ@u�h@u��@uzx@t��@s�m@s�@r:*@q�@q��@qIR@q(�@p�@p�4@pm�@o�P@oC@n�B@n~�@m�j@m�T@m��@mO�@mq@l�@l��@k��@k\)@jߤ@i�3@i;@h�Y@h%�@g��@f�@f��@f��@e�D@e�@e�=@e!�@d�e@dD�@d  @c�+@c� @cK�@b� @b#:@a�t@a \@`Ɇ@`�Y@_��@_]�@_�@^�@^��@^��@^}V@^^5@^&�@]��@]��@]*0@\�p@\��@\]d@[ƨ@[RT@Z��@Z?@Y@Y/@X�D@Xj@X�@W��@W�@V�h@V��@Vn�@V?@Uϫ@U�S@U�@T�@T��@T�Y@Tu�@T[�@T2�@T1@S�$@S@Rs�@RQ@R	@Q�9@Q��@Q�"@Q�7@Q(�@PɆ@P��@Pz�@P[�@Pb@O"�@N�1@N6�@M�H@Mk�@MG�@L��@Lq@K�
@K�$@J��@J&�@I��@I<6@H�@H�@H[�@H2�@H1@H/�@H�@G��@GZ�@G��@G]�@G�@F��@F�R@F�+@FQ@E��@E`B@E�@D�@Du�@C��@Cg�@C�@C�@B�'@Ba|@B�@A��@A*0@@Ɇ@@��@@tT@@H@?ݘ@?e�@?1�@?@>��@>��@>^5@=�@=(�@<��@<֡@<�U@<��@<D�@<(�@;�g@;qv@;P�@;C�@;;d@;1�@:�!@:Ta@:J@9��@9�3@9��@9�@9<6@8��@8��@8��@8�9@8]d@8G@7�m@7��@7��@7�{@7\)@7Y@6��@6Z�@6=q@6$�@6&�@5��@5}�@5w2@5|@5rG@5?}@5@@5	l@4oi@4%�@3�@3�@3e�@3C�@2��@2�1@2($@1�@1Q�@10�@1�@0�|@0�D@0?�@/��@/]�@/�@/S@.�@.L0@.�@-��@-�@-0�@,��@,��@+�m@+X�@+�@*��@*�\@*�@*�@)�D@)�@)o @)B�@(�`@(�@'�&@'�q@'�	@'X�@'&@&�]@&�}@&��@&#:@%\�@$��@$K^@$b@#�$@#J#@"��@"�,@"�@"W�@"�@!��@!�d@!��@!�~@!p�@!J�@!�@ �@ ��@ PH@��@�F@�f@Y@�H@��@_�@{@�@�d@�X@��@x�@x�@u�@hs@=�@@��@y>@D�@"h@�@��@�@��@��@C@��@��@��@R�@.�@��@��@�j@�@��@X@@�	@��@�?@��@�z@�.@y>@9X@�@�0@��@��@o�@]�@"�@�X@�!@��@ff@.�@4@J@�@��@�@�@�X@m]@O�@7L@�@�5@�4@tT@Xy@6@�r@خ@�6@�w@�:@s@Mj@
=@�<@��@�@�\@��@��@{�@s�@i�@R�@.�@��@��@�X@�~@\�@Dg@!�@֡@�4@�u@z�@j@D�@4n@"h@�@��@��@o�@.I@��@ߤ@�X@�A@GE@@�N@�@�h@��@p�@c�@Q�@L�@=�@%@z�@'R@�@��@ƨ@��@�[@�$@�	@l�@dZ@O@4�@�@
��@
��@
p;@
GE@
�@
J1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A�ŢA��}A���A�ޞA��A���A���A��dA���A���A��sA���A�یA��)A�ܒA��QA��5A���A��sA��aA��pA��aĄXA̞A̚kA̘�A̜ẠnA̅�A�K^A˿}A˛�A�e�A�#:A�S&A�QA�"hA��aA�_�A��A�b�A�ݘA���A��FA��qA��.A�8�A�ٴA��A���A���A��AA�O�A��TA�c�A�uA�g�A�r�A��A�=qA�T,A��~A�ߤA�ҽA��A��SA���A���A�S[A�v�A���A�S&A�\)A��A�
�A~(A|�+AwR�AtC�As�Aq1'An��Al<�Af��Ac�0A^Q�A]�AV�`AR�ANZAK?�AF�:ABYA?��A=bA<�A:�A7dZA55?A3�#A2˒A2��A1XA0/�A.S&A,�A*�A)�}A(�A()�A'�eA'(�A&jA%�pA$�;A#�A"*0A ��As�Ax�A�A�jA \A$AAe�A�A�dA8�AȴA��A��A,�A�A�A��A�A�hA�&A� AYA�fA��A��A�AU�AGA	��A��A�
A�EAb�A��A�DA�tA�#Ap;A�A ��@���@��t@���@��w@�M@��$@�n/@�s�@�1'@�s@���@��'@�� @�u%@��@��A s�A2aA�A��A ��A �aA +@��@��>@�a�@��@���@�i�@��@�2�@��=@�8@�?@�\�@��r@�zx@���@��@�,�@�S�@�q@�g�@@�Xy@��@���@�7�@�e@�{@�`B@�o@���@��@�@�,�@��D@�`�@���@�33@�qv@�C@�O�@��M@��@�	@�u�@��
@�a@��@��2@�`�@ߓ@�\�@�,�@޾@�?@�6�@��@�H�@�$t@�9�@���@��@�u%@�e@ۭC@��f@�_�@٢�@�S&@���@�z�@�"h@�Q�@��@�Xy@�4@ՠ�@�+�@��@ԝI@��Q@�j@�2a@�_@Ѻ^@�4@�h
@���@�dZ@�*0@̧�@�c�@̌�@��@�e@ˇ�@��@ʞ�@��@�˒@Ɏ"@��@��@ș1@�-�@�ƨ@��@ƌ@��@�A�@��@č�@�(�@Ò:@��@�ѷ@«6@�R�@���@��f@�-@���@��$@�Y�@�@O@�!�@��@�:*@��@�/�@��6@�C-@��}@�E9@���@��.@�R�@�Ov@��+@�s�@�/@�	l@��y@���@�W�@�+k@��@���@�C�@�@��I@�`�@��@�e,@�2a@�c�@�
�@���@��7@�E9@��@�� @�,=@��=@�!�@��@�W�@��;@��7@���@�z�@�ԕ@��@���@���@�>B@��a@���@��@�c @���@�@��?@�C�@���@��[@�(@�ff@���@�_@��j@��@�,�@���@�[�@�@�@���@��}@���@�-w@�J#@���@��!@��A@�Ov@��@���@���@�RT@��j@���@�.�@���@�A @��R@�Q�@�  @��@�U�@�S&@�6z@�+@�+@��@��O@���@�m�@�c @�9X@���@�~�@�v`@��	@�S�@�K�@�33@��@��[@�4n@��@��*@�]�@�;@���@�6@��F@�\)@��@���@�p;@�u@���@���@��@���@�zx@�F@��@�e@���@�s@�4�@��@�֡@���@�bN@���@�F�@��@���@��@�U2@�%�@��@�b@���@��@���@��@���@��b@��D@�a|@��@���@��@�A @��y@���@��@���@�t�@��@���@�3�@��:@�F�@��@�(�@�ȴ@�n�@�@��"@��M@��@���@���@��A@�_@�!@���@�qv@�[W@�J#@��@��}@�d�@��@�e,@�=�@�"�@���@���@�3�@���@��X@���@�G�@��|@��9@��x@��.@��@�M@��@���@�?}@��@���@�֡@��D@�R�@�+k@��@��P@�k�@�[W@�S�@�RT@�P�@�5�@�Ɇ@��@��A@�r�@�W�@�*�@�+@��@�$@,�@S@~��@}��@}[W@}%@|�v@|��@|��@}�@}��@}x�@|�e@|>B@{�@@{"�@z�}@z��@zxl@yu�@x�@x��@xoi@xg8@xXy@x$@w��@w�@v�@v�B@v��@v�F@uϫ@u�h@u��@uzx@t��@s�m@s�@r:*@q�@q��@qIR@q(�@p�@p�4@pm�@o�P@oC@n�B@n~�@m�j@m�T@m��@mO�@mq@l�@l��@k��@k\)@jߤ@i�3@i;@h�Y@h%�@g��@f�@f��@f��@e�D@e�@e�=@e!�@d�e@dD�@d  @c�+@c� @cK�@b� @b#:@a�t@a \@`Ɇ@`�Y@_��@_]�@_�@^�@^��@^��@^}V@^^5@^&�@]��@]��@]*0@\�p@\��@\]d@[ƨ@[RT@Z��@Z?@Y@Y/@X�D@Xj@X�@W��@W�@V�h@V��@Vn�@V?@Uϫ@U�S@U�@T�@T��@T�Y@Tu�@T[�@T2�@T1@S�$@S@Rs�@RQ@R	@Q�9@Q��@Q�"@Q�7@Q(�@PɆ@P��@Pz�@P[�@Pb@O"�@N�1@N6�@M�H@Mk�@MG�@L��@Lq@K�
@K�$@J��@J&�@I��@I<6@H�@H�@H[�@H2�@H1@H/�@H�@G��@GZ�@G��@G]�@G�@F��@F�R@F�+@FQ@E��@E`B@E�@D�@Du�@C��@Cg�@C�@C�@B�'@Ba|@B�@A��@A*0@@Ɇ@@��@@tT@@H@?ݘ@?e�@?1�@?@>��@>��@>^5@=�@=(�@<��@<֡@<�U@<��@<D�@<(�@;�g@;qv@;P�@;C�@;;d@;1�@:�!@:Ta@:J@9��@9�3@9��@9�@9<6@8��@8��@8��@8�9@8]d@8G@7�m@7��@7��@7�{@7\)@7Y@6��@6Z�@6=q@6$�@6&�@5��@5}�@5w2@5|@5rG@5?}@5@@5	l@4oi@4%�@3�@3�@3e�@3C�@2��@2�1@2($@1�@1Q�@10�@1�@0�|@0�D@0?�@/��@/]�@/�@/S@.�@.L0@.�@-��@-�@-0�@,��@,��@+�m@+X�@+�@*��@*�\@*�@*�@)�D@)�@)o @)B�@(�`@(�@'�&@'�q@'�	@'X�@'&@&�]@&�}@&��@&#:@%\�@$��@$K^@$b@#�$@#J#@"��@"�,@"�@"W�@"�@!��@!�d@!��@!�~@!p�@!J�@!�@ �@ ��@ PH@��@�F@�f@Y@�H@��@_�@{@�@�d@�X@��@x�@x�@u�@hs@=�@@��@y>@D�@"h@�@��@�@��@��@C@��@��@��@R�@.�@��@��@�j@�@��@X@@�	@��@�?@��@�z@�.@y>@9X@�@�0@��@��@o�@]�@"�@�X@�!@��@ff@.�@4@J@�@��@�@�@�X@m]@O�@7L@�@�5@�4@tT@Xy@6@�r@خ@�6@�w@�:@s@Mj@
=@�<@��@�@�\@��@��@{�@s�@i�@R�@.�@��@��@�X@�~@\�@Dg@!�@֡@�4@�u@z�@j@D�@4n@"h@�@��@��@o�@.I@��@ߤ@�X@�A@GE@@�N@�@�h@��@p�@c�@Q�@L�@=�@%@z�@'R@�@��@ƨ@��@�[@�$@�	@l�@dZ@O@4�@�@
��@
��@
p;@
GE@
�@
J1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
.�B
.B
-�B
.�B
.�B
.�B
.�B
.�B
/ B
.cB
.cB
.B
.IB
.IB
.�B
.�B
.�B
.�B
.�B
-�B
-�B
,=B
+�B
)yB
(sB
($B
'�B
(
B
(�B
&LB
!-B
.B

�B
B	�4B	� B	�B	��B	�B	�$B	��B	�8B	��B
�B
�B
WYB
d�B
zDB
r�B
rGB
o5B
lB
tnB
~B
zB
t�B
s�B
pUB
qB
r-B
�B
��B
͹B
��B
�SB
��B
vB
U�B
F�B
?B
@iB
.�B
:B	�}B	�B	�rB	��B	��B	�<B	vzB	qvB	e�B	U2B	K�B	?}B	33B	�B	�B		�B	+B��B�B��B��B�*B��B��B��B�&B��B�mB�B��B��B��B��B�B��B�qB��B��B�B�B� B��B��B�B�>B��B��B�B��B��B�B�BB��B�|B��B�_B��B��B�=B�B�B��B�hB�B��B��B�OB�nB��B�GB��B	
	B	B��B��B��B�B�OB�:B��B�tB��B�ZB�PB��B��B��B�BB�iB��B��B�4B�2B��B�vB�8B�wB��B��B��B��B	B	pB	3MB	@�B	V�B	`'B	a�B	cB	b�B	b�B	bNB	k�B	q'B	r-B	vzB	�3B	�PB	�NB	�MB	�sB	�B	��B	� B	��B	�rB	~�B	vB	tB	�JB	�"B	��B	}<B	u�B	}�B	|�B	{�B	��B	��B	�B	�B	�hB	�B	�MB	�B	�[B	��B	��B	��B	��B	�rB	��B	��B	�KB	��B	��B	�0B	�CB	��B	��B	��B	�|B	�B	��B	�`B	�B	�lB	�B	�cB	�oB	��B	��B	��B	ɺB	ɠB	�lB	�fB	��B	ɆB	��B	��B	��B	�B	āB	ŢB	�?B	ƎB	��B	��B	żB	��B	ŢB	�3B	��B	��B	�jB	��B	�%B	�`B	��B	�fB	�xB	�PB	��B	��B	�AB	�oB	�B	� B	��B	�[B	�-B	�gB	��B	ĜB	āB	��B	��B	�B	�B	�B	�YB	�EB	�fB	�lB	�lB	ɺB	�B	ɆB	ʦB	��B	̳B	�B	�B	�B	͹B	�B	�B	ѷB	��B	�&B	��B	�,B	�aB	�2B	��B	յB	ևB	׍B	��B	�B	�B	�yB	�KB	�B	�B	ٴB	ڠB	ڠB	�	B	�7B	�WB	�WB	�WB	��B	�xB	ܒB	��B	�B	�/B	�~B	ݘB	�OB	ޞB	��B	ߤB	�vB	��B	��B	�NB	�nB	�B	�B	��B	�B	�B	��B	��B	��B	��B	�(B	�wB	��B	��B	��B	�]B	��B
 �B
�B
uB
'B
aB
MB
SB
�B
�B
�B
9B
mB
�B
B
EB
_B
zB
�B
	�B
	�B
	RB
	7B

=B

	B
	RB
	7B
�B
�B
�B
B
	B
�B
	B
	7B
	RB
�B
�B
�B
dB
B
jB
�B
�B
6B
VB
.B
�B
�B
�B
oB
�B
 B
B
�B
&B
B
aB
�B
MB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
sB
sB
�B
�B
�B
�B
_B
�B
eB
�B
#B
#B
=B
�B
)B
�B
�B
~B
�B
�B
5B
�B
B
B
�B
OB
�B
!B
�B
 BB
!bB
!HB
!B
 �B
 vB
 'B
;B
pB
 BB
"�B
#TB
$@B
#�B
$ZB
#�B
$@B
%B
$�B
%�B
%�B
%�B
%�B
'B
&�B
&�B
&fB
%�B
%�B
&2B
&�B
&�B
&�B
'B
'mB
'�B
'�B
'�B
'�B
(>B
(�B
)�B
*B
*B
*0B
*0B
*0B
)�B
)yB
)�B
)�B
)�B
)�B
)�B
*B
*0B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+kB
+�B
+kB
+�B
+�B
+�B
,=B
,qB
,WB
,�B
,�B
,�B
-]B
-�B
-�B
.IB
.�B
/OB
1�B
3�B
3�B
3�B
3�B
33B
2�B
2�B
2�B
2�B
1�B
1AB
2GB
2�B
2�B
3MB
3�B
4TB
4�B
5%B
5%B
5�B
5�B
6B
6zB
6�B
6�B
8RB
9>B
8�B
8�B
8B
8�B
9	B
9$B
8�B
9>B
9�B
:DB
;B
;�B
;�B
<�B
<�B
=�B
>�B
>�B
?B
AB
?�B
@ B
?�B
>�B
>�B
>�B
?.B
?�B
@B
@iB
@�B
A B
A B
AUB
A�B
B'B
B�B
C�B
DMB
EB
EB
EB
E�B
E�B
F?B
E�B
E�B
F%B
FB
F%B
F%B
F�B
G+B
G�B
G�B
G�B
H1B
G�B
G�B
G�B
HfB
H�B
H�B
I7B
I�B
I�B
I�B
J=B
J�B
J�B
KDB
KDB
K�B
K�B
K�B
LB
L0B
L0B
MB
MjB
M�B
M�B
NB
NB
M�B
NVB
OB
O�B
OBB
N�B
N�B
O(B
O(B
OBB
PB
Q B
PbB
QhB
Q�B
RTB
RoB
R�B
S@B
S@B
SuB
S�B
TFB
T�B
T{B
TFB
SuB
R�B
R B
QNB
Q B
QB
QNB
Q�B
Q�B
R B
S�B
UB
T�B
T�B
U�B
XB
W�B
W�B
W�B
XB
XB
X�B
Y1B
X�B
YKB
YB
YB
Y�B
ZkB
Z�B
Z�B
[	B
[=B
[�B
[�B
\CB
\B
[�B
[�B
[�B
[�B
[�B
[WB
[WB
[=B
[#B
[	B
[=B
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
\)B
\�B
]/B
]dB
]~B
]~B
^5B
^OB
^�B
_;B
_VB
`�B
`vB
`B
`�B
aB
`�B
`�B
aB
a|B
a|B
a�B
a�B
bB
bhB
b�B
c�B
d&B
dZB
dtB
e�B
ezB
f2B
f2B
f2B
f�B
gB
gB
gB
g�B
g�B
g�B
g�B
h
B
g�B
h>B
h�B
h�B
h�B
iyB
i�B
iyB
i�B
j0B
jeB
j�B
j�B
j�B
jB
k6B
k�B
lB
l"B
lWB
l=B
l=B
l�B
l�B
mCB
m�B
m�B
m�B
nIB
n/B
m�B
nB
n}B
ncB
n�B
o5B
o�B
o�B
p!B
pUB
poB
p�B
p�B
p�B
p�B
q�B
rB
raB
raB
r�B
sB
s�B
s�B
s�B
s�B
tB
t9B
tnB
t�B
t�B
t�B
t�B
t�B
u?B
uZB
u�B
u�B
u�B
vFB
v�B
v�B
v�B
wLB
w�B
w�B
w�B
w�B
w�B
xB
w�B
xB
xB
xB
xlB
x�B
x�B
x�B
y	B
y	B
y$B
y	B
y>B
yrB
y�B
z*B
zDB
z�B
z�B
z�B
z�B
z�B
z�B
{B
{JB
{B
{�B
{�B
{�B
{�B
|B
|B
|B
|B
|jB
|�B
|�B
}B
}B
}B
}B
}qB
}�B
}�B
}�B
~(B
~BB
~wB
~]B
~�B
~�B
~�B
~�B
~�B
.B
B
HB
cB
cB
� B
�B
�B
�4B
�iB
��B
��B
��B
��B
��B
��B
�B
��B
�oB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�uB
�uB
�uB
��B
��B
��B
��B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�B
�B
�B
��B
��B
��B
�?B
�tB
�tB
�tB
��B
��B
��B
��B
��B
�B
��B
��B
��B
�1B
�KB
�fB
�KB
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
�	B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
.�B
.B
-�B
.�B
.�B
.�B
.�B
.�B
/ B
.cB
.cB
.B
.IB
.IB
.�B
.�B
.�B
.�B
.�B
-�B
-�B
,=B
+�B
)yB
(sB
($B
'�B
(
B
(�B
&LB
!-B
.B

�B
B	�4B	� B	�B	��B	�B	�$B	��B	�8B	��B
�B
�B
WYB
d�B
zDB
r�B
rGB
o5B
lB
tnB
~B
zB
t�B
s�B
pUB
qB
r-B
�B
��B
͹B
��B
�SB
��B
vB
U�B
F�B
?B
@iB
.�B
:B	�}B	�B	�rB	��B	��B	�<B	vzB	qvB	e�B	U2B	K�B	?}B	33B	�B	�B		�B	+B��B�B��B��B�*B��B��B��B�&B��B�mB�B��B��B��B��B�B��B�qB��B��B�B�B� B��B��B�B�>B��B��B�B��B��B�B�BB��B�|B��B�_B��B��B�=B�B�B��B�hB�B��B��B�OB�nB��B�GB��B	
	B	B��B��B��B�B�OB�:B��B�tB��B�ZB�PB��B��B��B�BB�iB��B��B�4B�2B��B�vB�8B�wB��B��B��B��B	B	pB	3MB	@�B	V�B	`'B	a�B	cB	b�B	b�B	bNB	k�B	q'B	r-B	vzB	�3B	�PB	�NB	�MB	�sB	�B	��B	� B	��B	�rB	~�B	vB	tB	�JB	�"B	��B	}<B	u�B	}�B	|�B	{�B	��B	��B	�B	�B	�hB	�B	�MB	�B	�[B	��B	��B	��B	��B	�rB	��B	��B	�KB	��B	��B	�0B	�CB	��B	��B	��B	�|B	�B	��B	�`B	�B	�lB	�B	�cB	�oB	��B	��B	��B	ɺB	ɠB	�lB	�fB	��B	ɆB	��B	��B	��B	�B	āB	ŢB	�?B	ƎB	��B	��B	żB	��B	ŢB	�3B	��B	��B	�jB	��B	�%B	�`B	��B	�fB	�xB	�PB	��B	��B	�AB	�oB	�B	� B	��B	�[B	�-B	�gB	��B	ĜB	āB	��B	��B	�B	�B	�B	�YB	�EB	�fB	�lB	�lB	ɺB	�B	ɆB	ʦB	��B	̳B	�B	�B	�B	͹B	�B	�B	ѷB	��B	�&B	��B	�,B	�aB	�2B	��B	յB	ևB	׍B	��B	�B	�B	�yB	�KB	�B	�B	ٴB	ڠB	ڠB	�	B	�7B	�WB	�WB	�WB	��B	�xB	ܒB	��B	�B	�/B	�~B	ݘB	�OB	ޞB	��B	ߤB	�vB	��B	��B	�NB	�nB	�B	�B	��B	�B	�B	��B	��B	��B	��B	�(B	�wB	��B	��B	��B	�]B	��B
 �B
�B
uB
'B
aB
MB
SB
�B
�B
�B
9B
mB
�B
B
EB
_B
zB
�B
	�B
	�B
	RB
	7B

=B

	B
	RB
	7B
�B
�B
�B
B
	B
�B
	B
	7B
	RB
�B
�B
�B
dB
B
jB
�B
�B
6B
VB
.B
�B
�B
�B
oB
�B
 B
B
�B
&B
B
aB
�B
MB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
sB
sB
�B
�B
�B
�B
_B
�B
eB
�B
#B
#B
=B
�B
)B
�B
�B
~B
�B
�B
5B
�B
B
B
�B
OB
�B
!B
�B
 BB
!bB
!HB
!B
 �B
 vB
 'B
;B
pB
 BB
"�B
#TB
$@B
#�B
$ZB
#�B
$@B
%B
$�B
%�B
%�B
%�B
%�B
'B
&�B
&�B
&fB
%�B
%�B
&2B
&�B
&�B
&�B
'B
'mB
'�B
'�B
'�B
'�B
(>B
(�B
)�B
*B
*B
*0B
*0B
*0B
)�B
)yB
)�B
)�B
)�B
)�B
)�B
*B
*0B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+kB
+�B
+kB
+�B
+�B
+�B
,=B
,qB
,WB
,�B
,�B
,�B
-]B
-�B
-�B
.IB
.�B
/OB
1�B
3�B
3�B
3�B
3�B
33B
2�B
2�B
2�B
2�B
1�B
1AB
2GB
2�B
2�B
3MB
3�B
4TB
4�B
5%B
5%B
5�B
5�B
6B
6zB
6�B
6�B
8RB
9>B
8�B
8�B
8B
8�B
9	B
9$B
8�B
9>B
9�B
:DB
;B
;�B
;�B
<�B
<�B
=�B
>�B
>�B
?B
AB
?�B
@ B
?�B
>�B
>�B
>�B
?.B
?�B
@B
@iB
@�B
A B
A B
AUB
A�B
B'B
B�B
C�B
DMB
EB
EB
EB
E�B
E�B
F?B
E�B
E�B
F%B
FB
F%B
F%B
F�B
G+B
G�B
G�B
G�B
H1B
G�B
G�B
G�B
HfB
H�B
H�B
I7B
I�B
I�B
I�B
J=B
J�B
J�B
KDB
KDB
K�B
K�B
K�B
LB
L0B
L0B
MB
MjB
M�B
M�B
NB
NB
M�B
NVB
OB
O�B
OBB
N�B
N�B
O(B
O(B
OBB
PB
Q B
PbB
QhB
Q�B
RTB
RoB
R�B
S@B
S@B
SuB
S�B
TFB
T�B
T{B
TFB
SuB
R�B
R B
QNB
Q B
QB
QNB
Q�B
Q�B
R B
S�B
UB
T�B
T�B
U�B
XB
W�B
W�B
W�B
XB
XB
X�B
Y1B
X�B
YKB
YB
YB
Y�B
ZkB
Z�B
Z�B
[	B
[=B
[�B
[�B
\CB
\B
[�B
[�B
[�B
[�B
[�B
[WB
[WB
[=B
[#B
[	B
[=B
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
\)B
\�B
]/B
]dB
]~B
]~B
^5B
^OB
^�B
_;B
_VB
`�B
`vB
`B
`�B
aB
`�B
`�B
aB
a|B
a|B
a�B
a�B
bB
bhB
b�B
c�B
d&B
dZB
dtB
e�B
ezB
f2B
f2B
f2B
f�B
gB
gB
gB
g�B
g�B
g�B
g�B
h
B
g�B
h>B
h�B
h�B
h�B
iyB
i�B
iyB
i�B
j0B
jeB
j�B
j�B
j�B
jB
k6B
k�B
lB
l"B
lWB
l=B
l=B
l�B
l�B
mCB
m�B
m�B
m�B
nIB
n/B
m�B
nB
n}B
ncB
n�B
o5B
o�B
o�B
p!B
pUB
poB
p�B
p�B
p�B
p�B
q�B
rB
raB
raB
r�B
sB
s�B
s�B
s�B
s�B
tB
t9B
tnB
t�B
t�B
t�B
t�B
t�B
u?B
uZB
u�B
u�B
u�B
vFB
v�B
v�B
v�B
wLB
w�B
w�B
w�B
w�B
w�B
xB
w�B
xB
xB
xB
xlB
x�B
x�B
x�B
y	B
y	B
y$B
y	B
y>B
yrB
y�B
z*B
zDB
z�B
z�B
z�B
z�B
z�B
z�B
{B
{JB
{B
{�B
{�B
{�B
{�B
|B
|B
|B
|B
|jB
|�B
|�B
}B
}B
}B
}B
}qB
}�B
}�B
}�B
~(B
~BB
~wB
~]B
~�B
~�B
~�B
~�B
~�B
.B
B
HB
cB
cB
� B
�B
�B
�4B
�iB
��B
��B
��B
��B
��B
��B
�B
��B
�oB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�uB
�uB
�uB
��B
��B
��B
��B
�{B
�{B
�{B
��B
��B
��B
��B
��B
��B
�B
��B
��B
��B
�B
�B
�B
��B
��B
��B
�?B
�tB
�tB
�tB
��B
��B
��B
��B
��B
�B
��B
��B
��B
�1B
�KB
�fB
�KB
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
�	B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230608124741  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230608124749  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230608124750  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230608124750                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230608124751  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230608124751  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230608125710                      G�O�G�O�G�O�                