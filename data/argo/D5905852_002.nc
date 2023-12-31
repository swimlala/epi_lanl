CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-05-23T10:00:15Z creation;2019-05-23T10:00:16Z conversion to V3.1;2022-08-02T05:13:01Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ހ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190523100015  20220818081507  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_002                    2C  D   APEX                            8420                            2.11.2                          846 @ة&8��1   @ة&����@*�1&�y�d�8�4֡1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BrffBv��B���B���B�  B���B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���BÙ�B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C�fC  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2ffC3��C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @
=@|(�@�  A Q�A33A?�A_�A�
A�{A��A��A��A�  A��
A��A��B�B�B�HBB'B/�HB7��B@
=BH{BP{BX  B_�
Bg�HBrffBv��B��{B��RB���B���B�8RB�L�B��
B��fB��B���B���B��B���B��B���B�  B��{B�z�B���B��HB��)B��fB��B��B��B��HB��B�B��B���B�  B�C C�C�qC{C  C	�RC�qCC�3C�C�RC  C  C�C��C��C��C!�qC#�qC%��C(  C*  C,�C.C0�C2\)C3�\C5�qC7�qC9�qC;�qC>  C?��CA�qCC��CE�3CG�qCI��CK��CM�RCO�qCQ�RCS��CU��CW�RCY�RC[�RC]��C_�RCa��Cc��Ce��Ch�Ci�qCl  CnCp  CrCt�Cu�qCw��Cy�3C{��C}�RC��C���C��RC��qC���C��qC���C��)C�HC��C��qC���C�  C��qC���C�  C���C��)C���C���C��RC��)C��)C���C���C��)C��qC��qC��qC��qC���C��qC��)C��C�HC��)C���C���C��qC�  C��qC���C�  C�HC���C���C��)C�  C��)C���C�HC��qC��RC���C�HC���C���C���C���C���C���C�  C��qC���C���C��qC�  C�  C��qC��)C��qC���C���C���C���C���C��qC�  C�HC�  C�  C�  C�  C���C��qC��qC��qC��)C��)C��qC���C��qC���C��RC���C��)C��qC�  C�HC��C��)C���C��qC��qC��qC���C���C���C���C���C��C�  C���C���C��qC��qC�HC�HC���C�  C���C���C��qC���C���C���C��qC��D �D \D �\D~�D�\D� D�\D~�D�qD}qD�D}qD��D\D�\D� D��D}qD��D	~D	�qD
~�D
�D~D��D~�D�\D~�D��D|�D�D\D��D}qD�qD}qD��D~�D�\D��D �D\D��D}qD�\D\D �D� D�\D~�D��D~�D �D~�D�qD~D�D~�D  D\D��D~D��D\D�\D ~D �\D!� D" �D"~D"�qD#~D#�qD$� D$�\D%~�D%�qD&|�D&��D'}qD'�qD(}qD(�D)~D*  D*��D+ �D+~D+��D,|)D,�D-� D-��D.|�D.�qD/~�D/�)D0}qD0�D1|�D1��D2��D3�D3� D3��D4~�D4�\D5\D6  D6\D6�qD7}qD7��D8� D8��D9\D9��D:\D:�\D;~�D;��D<|�D<�)D=~D=��D>~�D>�\D?}qD?�D@~D@��DA{�DA�)DB~�DC �DC� DD  DD��DD�\DE\DF �DF��DGHDG� DG��DH~�DIHDI~�DI�qDJ� DJ�\DK}qDK�qDL~�DM �DM� DM�)DN|�DN��DO~DO�qDP|�DP�)DQ~DR  DR�HDS �DS}qDS��DT|�DT��DU}qDU��DV��DV�\DW~DX �DX\DX�)DY~DY�\DZ|)DZ�qD[\D[��D\~D\��D]|�D]�)D^|�D^�)D_{�D_�D`��D`�Da\Db  Db}qDb��Dc��Dc�Dd}qDd��De��De�qDf}qDf��Dg\Dg�\Dh|)Dh��Di\Di�\Dj~Dk �Dk\Dk��Dl|�Dl�)Dm{�Dm��Dn�HDn�\Do}qDo�)Dp}qDp�qDq|�Dq�Dr��Ds  Ds~Ds�qDt~Dt�\Du~�Du�qDv~�Dw �Dw��Dx �Dx\Dx�qDy}qDy�Dz~�Dz��D{~�D{�D|~D|�D}~�D}�D~|�D~�D~D�D�>�D��D��\D��
D�?
D�
D���D�  D�AHD���D���D�  D�?�D�
D��\D���D�?�D��D��\D��
D�?
D�� D��\D��\D�?
D�~�D��fD��\D�?\D�~�D��fD��
D�?�D��RD��RD�  D�>�D�~�D�� D�  D�?�D�� D��RD��
D�?\D��RD��RD���D�?
D��D��RD��
D�>fD��D��RD��\D�>�D�\D���D���D�?�D��D��D��fD�?
D��D��
D���D�?�D�\D��
D�  D�@ D�~fD��fD���D�?�D�
D���D� �D�?
D��D��RD��\D�>�D�\D��RD��\D�>�D�~D��
D�  D�?\D�� D���D�  D�?�D�� D��RD��\D�?
D��D��\D��
D�?\D�
D���D���D�@ D�\D�� D��
D�?
D�
D���D��
D�@ D�� D���D���D�?
D�\D���D��fD�?\D��D��
D���D�>fD�~D��D��
D�?�D�\D��fD��\D�?�D�~�D��fD��
D�?\D��D�� D��\D�?
D�\D��fD���D�>�D�~�D���D���D�>fD�
D���D���D�@RD��D���D���D�>D�~fD���D� RD�?�D��D��\D��\D�@RD�� D���D� RD�?�D�� D�� D� �D�@RD�� D��RD���D�=�D�~fD���D��
D�?�D�\D��
D��\D�=�D�~fD��\D�  D�@�D�� D�� D�  D�>fD�
D���D��fD�>�D��D��RD�  D�?\D��D�� D�  D�?�D�\D���D�  D�?\D�~�D��
D��\D�>�D�
D��\D��
D�@ D���D���D���D�?
D��D��RD�  D�>�D�\D��RD�  D�?
D�
D��
D���D�@ D�� D���D��
D�>�D�\D���D��fD�>�D�\D��
D��
D�?�D�� D���D���D�?
D��D��RD���D�?\D�
D��
D�  D�@�D�\D���D��
D�?
D�D¿�D��\D�@RDÀRDÿ�D��\D�?\DĀ D�� D��\D�?\Dŀ D���D� �D�?�D��Dƿ\D��\D�?�D�\Dǿ�D���D�?\D�~fDȿ
D���D�@ Dɀ�D��RD�  D�?
D�\Dʿ
D��\D�?�D�~D˿\D� RD�@ D�
D̾�D��
D�?
D�\DͿ�D�  D�?
D�~�DξD���D�>�D�~�DϾfD��\D�@RDЀ�D���D��
D�>fD�~fDѾD���D�@�D�\DҾ�D��fD�>D�\Dӿ�D���D�@RD�\DԿ�D���D�>fD�
Dվ�D��
D�?�D�
D־fD��fD�>�D�
D׿�D� RD�?�D��Dؿ�D��
D�?\D�
Dٿ
D��\D�=�D�~Dھ�D��
D�?�D��Dۿ�D�  D�@�D�\DܾfD��\D�>�D�~fDݾ�D��
D�?
D�\D޿�D���D�?\D߀RD߿\D�  D�@�D�\D࿮D� RD�?�D�\D΅D��\D�?\D�\D⾸D��\D�@ D�\D㾸D���D�>fD�~D�D��\D�>�D�~fD忮D��\D�?
D��D濮D���D�?\D�~fD�
D�  D�?�D�~�D迮D��\D�?
D�~�D�\D���D�>�D�~D�D���D�?�D�RD뿮D��
D�@ D��D쾸D��
D�?
D�~D���D���D�?�D�RD�� D��
D�?
D�~D�
D� RD�@RD�
D�D��
D�?�D�\D�\D���D�?�D� D�� D��\D�?\D�RD���D���D�?
D� D��\D���D�>�D�~�D���D���D�>�D�~fD��fD��fD�>�D�
D��\D�  D�?
D�~�D��RD� �D�@ D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A��UA��A��jA��dA��HA��TA��A���A���A��)A��#A�ɆA̫�Ạ�A̓A̎"ÅrÂ�A�v�A�t�ÃGȦ�Ȁ�A̛qA̱'A���A� 'A�xA��.A���A��A��JA�'�A�]�A�EmA��]A���A��zA���A��A���A���A��EA���A�1[A�\A�w�A��A��hA�cTA��|A�d�A��hA���A��A�VA���A�T�A�o�A�a�A�P�A�y�A�)�A�t�A�;�A�-�A�P}A�i�A��A�!A��;A��-A�ƨA��?A�?}A�҉A���A��FA�m)A� iA��A�C�A��RA~��A|��Az�UAyK�At�&Ap�.Ad�BAbL�Aa�	A`یA_x�A^ƨA^+A]�yA\�:AX�|AVW�AUi�ATOARY�AO��AMj�AK|�AJ6AG�)AC�>AA2�A?�HA>��A=��A=FA;�}A9iDA5��A5h�A4N�A3�mA2�.A2e�A1zxA0��A.@OA-��A-X�A,o�A+�A'�3A&��A%ϫA%2aA$�	A#;dA!ѷA �A�9A��A�A8A-wA�AA�A^�AdZA�=A��A͟A�}A��A��A�@A��A�\A8AA�ZAbAK^A��AS�A��A\)A�A)_Ae,A�RAخA�'AqA�'A�A�A��A�0A�HA�fA��A�A�A�A
=A�A��A��A�>A�AA��A��A�:AX�A�AcA�A7LAA��AqA �Ak�A
�A	�A	0�AkQA~(A��A��A�4A  AD�A��A��AoiA��A��A?A&�A�)A��A��A�FA��AQ�A�A  A~A��AX�AL0A9XA$�AA `�A  �@��@���@�L0@��A@�v`@���@�a|@�Dg@�W?@���@�j@���@���@��@���@�a�@��`@�7�@�@��2@�0@�@�J@�:�@�N�@��@�j�@�	@�S@�?�@���@鯸@�c�@�oi@���@�o @�@�(�@�ϫ@��@���@�?@���@�PH@�;�@��@�C@�Y@��@�҉@���@��?@�L@��@ߎ�@�X�@�;@�@��&@�L�@��@���@�8�@��@��@�y�@�;@�h
@�d�@� �@�7L@��@��@��@֭�@�~�@���@�:�@��@��@Ե�@�u�@���@�s@�V@�l�@�ƨ@а�@�s�@��@͔�@́�@̀4@�n/@̽<@˃{@�<6@ʇ�@�ԕ@Ɏ"@�$t@ȕ�@ǲ-@�e�@��@ƛ�@�6�@��@ŖS@�?}@Į}@�v�@�U2@�~@ä@@¾�@+@�C�@�v`@���@��x@�Ov@��D@���@��@��@�q�@�9X@���@�;@�_@��@�o@���@�J�@��@��@��{@��@�i�@�4n@�!@���@���@�r�@���@��}@��F@���@��~@�c@�Y�@�-w@���@�j@��@��m@�5�@��1@�~@�c@��R@�(�@�J@���@��@��D@�c @�8�@���@�a@�8@�+�@�@��@�ߤ@�0U@��@��@�N<@�4�@��@���@���@��K@�~�@��:@�^�@�&@�	l@��@�҉@���@���@�z�@�J�@�{@�
�@��@�G@�G@��@��@��@�c@���@�C�@�O@�1@���@��@���@��4@�'�@���@���@��@��|@��Y@�g8@�R�@�O@���@�A @�6z@� \@��U@���@�[�@��@��&@�� @���@�Dg@��@��@��@�@���@��s@��@�"h@���@�\�@���@�8�@��@�J@�4@�  @��@�Mj@���@�z�@�M@���@��@�Ĝ@��Y@���@��~@� i@��e@���@���@�Z@�($@�� @�zx@��@�[�@�H�@�!�@��@�_@��&@�Y�@�@���@���@�oi@��@��9@���@��@��?@��@�p;@��@���@�x@�� @���@�Q�@�-w@��@��@���@�_@���@���@�e�@�@��@��'@�z�@�W�@��@�33@��@�͟@�q�@��D@�*0@�@��j@���@�}�@��@��|@��p@���@��_@�z�@�ff@�9X@��@��@��C@���@�  @(@~ȴ@~�@~l�@~J@}�#@}�@}%@|�[@{��@x�e@w��@w�@v�@vC�@u�@u��@s�]@sC�@r��@q�@q7L@p��@pH@o��@o��@n�@n~�@n�@m�Z@m}�@l�@k�*@kH�@k.I@k�@k�@j҉@j^5@i|@h֡@g�@g\)@fO@e(�@d6@ca@c�@c i@b�H@b�B@bȴ@b��@b�m@b��@b��@b�@b�F@bs�@bkQ@b&�@a�)@a��@a&�@`�@`��@`]d@`[�@`[�@`bN@`<�@_�@_�:@_\)@^ں@^�\@]��@]��@]s�@]@@\r�@\�@[�6@[S�@Z��@Z�}@Z��@Zh
@ZC�@Z3�@Z�@Y��@Y�@Y�@YDg@YA @Y<6@X�@W�P@WS�@V��@V8�@U�Z@UL�@T��@Tѷ@T�e@T�@T:�@S��@Sb�@S�@Rߤ@Q��@Q��@Q��@Q��@QS&@Q:�@Q*0@P��@Pی@P�[@P��@PĜ@P�@P��@PD�@P'R@P"h@O�@OX�@O.I@OC@O�@N��@N҉@NZ�@L��@Lu�@LS�@L9X@K�A@K��@K��@K�P@K�	@K|�@Kx@Ky�@Ky�@Kb�@K�@J�8@J��@J;�@Ia�@H9X@F��@F#:@E�.@E��@E��@E��@E��@Em]@EO�@E+@D��@D�`@D~(@DU2@D�@C{J@C@B�"@B��@B�8@B��@A��@@�@@�.@@"h@?�@?W?@>��@>�x@>� @>~�@>s�@>Ta@>M�@>Ov@>L0@>8�@>)�@=p�@=@<�@<��@<�U@<�O@<�u@<��@<M@;�6@;�@@;��@;��@;��@;@O@;�@:�}@9��@9B�@8�@8��@8�9@8��@8�_@8w�@8m�@8K^@8C-@8�@7�r@7�@7�@7��@7�4@7o�@7O@6ں@5�j@5S&@5*0@4�@4@3��@3��@3"�@2�@2v�@2�@1�@1�d@1��@1Vm@0��@0�E@0�@0:�@/�A@/�@/�;@/�Q@/ƨ@/�0@/��@/��@/;d@.��@.ߤ@.��@.:*@.�@-�D@-�@-�-@-p�@,��@+�@+Z�@+E9@+H�@+J#@+K�@+Mj@+E9@+9�@+4�@+/�@+'�@+"�@+�@+�@+�@+S@*�@*�@*�@*�@*��@*�@*�!@*1�@)��@)f�@)L�@)8�@)5�@)&�@)q@)%@(Ĝ@(�@(Ft@((�@'��@'�$@'4�@'S@&�@&�@&h
@&a|@&\�@&M�@&!�@%�@%u�@$�@$`�@$[�@$]d@$M@$?�@$ �@#�K@#o�@#�@"��@"�B@"� @"��@"+k@!��@!Vm@!4@!�@ ��@ 6@�@��@��@�@�@1�@�@��@w2@/@��@H@خ@g�@'�@'�@$t@o@�H@��@
�@�@c�@S&@Vm@Y�@S&@Q�@7L@�@�	@��@�.@!@��@�@��@j�@Mj@@��@�1@kQ@-@�@�z@a�@;@��@�e@�Y@1'@�@�@x@�@��@�@�0@�@H�@�@��@�@��@��@�'@�@��@�<@�@l�@_@\�@/@�@�/@Ĝ@��@K^@	�@�&@�6@��@�F@s@Mj@6z@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A��UA��A��jA��dA��HA��TA��A���A���A��)A��#A�ɆA̫�Ạ�A̓A̎"ÅrÂ�A�v�A�t�ÃGȦ�Ȁ�A̛qA̱'A���A� 'A�xA��.A���A��A��JA�'�A�]�A�EmA��]A���A��zA���A��A���A���A��EA���A�1[A�\A�w�A��A��hA�cTA��|A�d�A��hA���A��A�VA���A�T�A�o�A�a�A�P�A�y�A�)�A�t�A�;�A�-�A�P}A�i�A��A�!A��;A��-A�ƨA��?A�?}A�҉A���A��FA�m)A� iA��A�C�A��RA~��A|��Az�UAyK�At�&Ap�.Ad�BAbL�Aa�	A`یA_x�A^ƨA^+A]�yA\�:AX�|AVW�AUi�ATOARY�AO��AMj�AK|�AJ6AG�)AC�>AA2�A?�HA>��A=��A=FA;�}A9iDA5��A5h�A4N�A3�mA2�.A2e�A1zxA0��A.@OA-��A-X�A,o�A+�A'�3A&��A%ϫA%2aA$�	A#;dA!ѷA �A�9A��A�A8A-wA�AA�A^�AdZA�=A��A͟A�}A��A��A�@A��A�\A8AA�ZAbAK^A��AS�A��A\)A�A)_Ae,A�RAخA�'AqA�'A�A�A��A�0A�HA�fA��A�A�A�A
=A�A��A��A�>A�AA��A��A�:AX�A�AcA�A7LAA��AqA �Ak�A
�A	�A	0�AkQA~(A��A��A�4A  AD�A��A��AoiA��A��A?A&�A�)A��A��A�FA��AQ�A�A  A~A��AX�AL0A9XA$�AA `�A  �@��@���@�L0@��A@�v`@���@�a|@�Dg@�W?@���@�j@���@���@��@���@�a�@��`@�7�@�@��2@�0@�@�J@�:�@�N�@��@�j�@�	@�S@�?�@���@鯸@�c�@�oi@���@�o @�@�(�@�ϫ@��@���@�?@���@�PH@�;�@��@�C@�Y@��@�҉@���@��?@�L@��@ߎ�@�X�@�;@�@��&@�L�@��@���@�8�@��@��@�y�@�;@�h
@�d�@� �@�7L@��@��@��@֭�@�~�@���@�:�@��@��@Ե�@�u�@���@�s@�V@�l�@�ƨ@а�@�s�@��@͔�@́�@̀4@�n/@̽<@˃{@�<6@ʇ�@�ԕ@Ɏ"@�$t@ȕ�@ǲ-@�e�@��@ƛ�@�6�@��@ŖS@�?}@Į}@�v�@�U2@�~@ä@@¾�@+@�C�@�v`@���@��x@�Ov@��D@���@��@��@�q�@�9X@���@�;@�_@��@�o@���@�J�@��@��@��{@��@�i�@�4n@�!@���@���@�r�@���@��}@��F@���@��~@�c@�Y�@�-w@���@�j@��@��m@�5�@��1@�~@�c@��R@�(�@�J@���@��@��D@�c @�8�@���@�a@�8@�+�@�@��@�ߤ@�0U@��@��@�N<@�4�@��@���@���@��K@�~�@��:@�^�@�&@�	l@��@�҉@���@���@�z�@�J�@�{@�
�@��@�G@�G@��@��@��@�c@���@�C�@�O@�1@���@��@���@��4@�'�@���@���@��@��|@��Y@�g8@�R�@�O@���@�A @�6z@� \@��U@���@�[�@��@��&@�� @���@�Dg@��@��@��@�@���@��s@��@�"h@���@�\�@���@�8�@��@�J@�4@�  @��@�Mj@���@�z�@�M@���@��@�Ĝ@��Y@���@��~@� i@��e@���@���@�Z@�($@�� @�zx@��@�[�@�H�@�!�@��@�_@��&@�Y�@�@���@���@�oi@��@��9@���@��@��?@��@�p;@��@���@�x@�� @���@�Q�@�-w@��@��@���@�_@���@���@�e�@�@��@��'@�z�@�W�@��@�33@��@�͟@�q�@��D@�*0@�@��j@���@�}�@��@��|@��p@���@��_@�z�@�ff@�9X@��@��@��C@���@�  @(@~ȴ@~�@~l�@~J@}�#@}�@}%@|�[@{��@x�e@w��@w�@v�@vC�@u�@u��@s�]@sC�@r��@q�@q7L@p��@pH@o��@o��@n�@n~�@n�@m�Z@m}�@l�@k�*@kH�@k.I@k�@k�@j҉@j^5@i|@h֡@g�@g\)@fO@e(�@d6@ca@c�@c i@b�H@b�B@bȴ@b��@b�m@b��@b��@b�@b�F@bs�@bkQ@b&�@a�)@a��@a&�@`�@`��@`]d@`[�@`[�@`bN@`<�@_�@_�:@_\)@^ں@^�\@]��@]��@]s�@]@@\r�@\�@[�6@[S�@Z��@Z�}@Z��@Zh
@ZC�@Z3�@Z�@Y��@Y�@Y�@YDg@YA @Y<6@X�@W�P@WS�@V��@V8�@U�Z@UL�@T��@Tѷ@T�e@T�@T:�@S��@Sb�@S�@Rߤ@Q��@Q��@Q��@Q��@QS&@Q:�@Q*0@P��@Pی@P�[@P��@PĜ@P�@P��@PD�@P'R@P"h@O�@OX�@O.I@OC@O�@N��@N҉@NZ�@L��@Lu�@LS�@L9X@K�A@K��@K��@K�P@K�	@K|�@Kx@Ky�@Ky�@Kb�@K�@J�8@J��@J;�@Ia�@H9X@F��@F#:@E�.@E��@E��@E��@E��@Em]@EO�@E+@D��@D�`@D~(@DU2@D�@C{J@C@B�"@B��@B�8@B��@A��@@�@@�.@@"h@?�@?W?@>��@>�x@>� @>~�@>s�@>Ta@>M�@>Ov@>L0@>8�@>)�@=p�@=@<�@<��@<�U@<�O@<�u@<��@<M@;�6@;�@@;��@;��@;��@;@O@;�@:�}@9��@9B�@8�@8��@8�9@8��@8�_@8w�@8m�@8K^@8C-@8�@7�r@7�@7�@7��@7�4@7o�@7O@6ں@5�j@5S&@5*0@4�@4@3��@3��@3"�@2�@2v�@2�@1�@1�d@1��@1Vm@0��@0�E@0�@0:�@/�A@/�@/�;@/�Q@/ƨ@/�0@/��@/��@/;d@.��@.ߤ@.��@.:*@.�@-�D@-�@-�-@-p�@,��@+�@+Z�@+E9@+H�@+J#@+K�@+Mj@+E9@+9�@+4�@+/�@+'�@+"�@+�@+�@+�@+S@*�@*�@*�@*�@*��@*�@*�!@*1�@)��@)f�@)L�@)8�@)5�@)&�@)q@)%@(Ĝ@(�@(Ft@((�@'��@'�$@'4�@'S@&�@&�@&h
@&a|@&\�@&M�@&!�@%�@%u�@$�@$`�@$[�@$]d@$M@$?�@$ �@#�K@#o�@#�@"��@"�B@"� @"��@"+k@!��@!Vm@!4@!�@ ��@ 6@�@��@��@�@�@1�@�@��@w2@/@��@H@خ@g�@'�@'�@$t@o@�H@��@
�@�@c�@S&@Vm@Y�@S&@Q�@7L@�@�	@��@�.@!@��@�@��@j�@Mj@@��@�1@kQ@-@�@�z@a�@;@��@�e@�Y@1'@�@�@x@�@��@�@�0@�@H�@�@��@�@��@��@�'@�@��@�<@�@l�@_@\�@/@�@�/@Ĝ@��@K^@	�@�&@�6@��@�F@s@Mj@6z@o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	5�B	5�B	5�B	5�B	5?B	5%B	5%B	4�B	5%B	5?B	5?B	5?B	8�B	<�B	>]B	?�B	?�B	?}B	@4B	A�B	E�B	J�B	L~B	R�B	Y�B	c�B	sMB	�+B	��B	��B	��B	��B
�B
�B+B]�B{�B~�B�eB��B��B�lBĶB�B�8B�BBB,B�B/�B1�B1�B2�B5B4�B2�B.}B5tB49B/�B&LBB�B�BB�B��B�B�`B�^BtBc�B0oB
��B
�[B
��B
�B
r�B
[	B
J	B
,qB
�B
SB	��B	�B	�B	��B	�B	�0B	��B	��B	�%B	��B	��B	��B	��B	�gB	�GB	�-B	��B	�lB	�B	��B	�=B	��B	�%B	�B	}"B	r|B	poB	hXB	cTB	\�B	\�B	[qB	\�B	]�B	^B	^B	_�B	`B	_�B	]�B	`�B	_!B	YeB	\)B	]�B	_�B	u%B	v�B	s�B	s�B	qvB	}�B	��B	��B	�B	��B	��B	��B	�@B	�B	��B	��B	�B	��B	��B	��B	�FB	�2B	��B	�>B	�XB	�B	��B	�pB	ՁB	��B	��B	��B	ɺB	��B	��B	�}B	�iB	�B	�mB	˒B	�TB	��B	�yB	� B	�NB	ӏB	��B	�B	�`B	�8B	�>B	��B	�CB	�/B	�B	�/B	�B	��B	��B	�lB	�B	��B	��B	�6B	��B	��B	�8B
�B
:B
NB
�B
�B
~B
�B	�"B	�B	�ZB	�aB	��B	�kB	�kB	�KB	�B	�?B	�B	�B	��B	��B	�6B	�fB	�B	��B	�>B	�mB	�B	�B	�*B	�*B	�6B	�9B	�'B	��B	��B	�vB	�B	��B	�B	�+B	��B	�rB	��B	�lB	��B	�%B	�hB	�?B	�*B	�BB	��B	��B	�0B	��B	�PB	�6B	�JB	�*B	�>B	�B	�zB	��B	��B	��B	�2B	��B	�2B	�LB	��B	��B	�B	��B	��B	�LB	��B	��B	�2B	�LB	�fB	�fB	��B	��B	�B	��B	��B	��B	��B	��B	�LB	�2B	�fB	�B	�B	��B	�PB	��B	��B	��B	�B	��B	��B	�lB	��B	��B	�$B	�	B	��B	�$B	�xB	�0B	�0B	��B	��B	��B	�^B	��B	��B	�wB	��B	�.B	��B
 B
aB
�B
B
SB
B
YB
_B
fB
�B
�B
+B
�B
�B
�B
�B
�B
tB
B
%B
?B
B
�B
�B
�B
�B
B
�B
B
_B
�B
YB
�B
�B
YB
B
�B
�B
�B
�B
�B
�B
�B
B
EB
�B
_B
EB
�B
_B
�B
YB
B
9B
�B
�B
B
	B

#B
	�B
	�B
	�B
�B
PB
B
"B
<B
<B
"B
B
�B
PB
jB
�B
�B
�B
B
�B
�B
dB
�B
B
�B
�B
<B
VB
<B
<B
(B
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
\B
}B
�B
�B
4B
B
$B
$B
YB
YB
sB
�B
�B
+B
EB
EB
EB
EB
EB
+B
+B
B
�B
�B
�B
+B
B
B
�B
�B
�B
�B
�B
_B
�B
B
=B
�B
�B
kB
�B
	B
kB
7B
B
QB
B
B
�B
7B
kB
�B
�B
�B
qB
qB
WB
qB
qB
�B
CB
CB
/B
/B
/B
/B
B
dB
�B
jB
�B
�B
�B
�B
pB
!B
 �B
 B
 �B
 �B
 �B
!B
 �B
 �B
 �B
 �B
!-B
!|B
"4B
"hB
"hB
"�B
"�B
"hB
"NB
#nB
#�B
#�B
$ZB
$ZB
%B
%FB
%�B
%�B
&LB
&fB
&2B
'B
'�B
(�B
(�B
(�B
)B
)DB
)_B
)yB
)�B
)�B
*eB
*�B
+B
+kB
+kB
+kB
+�B
+�B
,�B
-�B
-�B
.B
.cB
.}B
/�B
0�B
1B
1AB
1[B
1�B
2B
2GB
2GB
2|B
2|B
2�B
2�B
2�B
2�B
2�B
4�B
5%B
5�B
5�B
5�B
5�B
6+B
6B
6`B
6`B
6B
6�B
8�B
9>B
9>B
9XB
9�B
9�B
9�B
;B
:�B
;dB
;�B
<6B
<�B
<�B
<�B
=<B
=VB
=�B
>B
=�B
>BB
>�B
?cB
?cB
?cB
?cB
?HB
?cB
?cB
@iB
@OB
A;B
A B
BB
B'B
C-B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
DB
D�B
D�B
D�B
EB
EB
D�B
EB
EB
ESB
ESB
EmB
E�B
E�B
FYB
FYB
FtB
F�B
G+B
GzB
G�B
H�B
H�B
IB
I7B
IRB
IRB
IlB
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
K)B
KB
K�B
K�B
LB
L�B
L�B
L�B
L�B
L�B
M6B
MjB
MjB
M�B
M�B
N�B
N�B
N�B
O(B
OvB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P}B
P�B
P�B
P�B
P�B
P}B
P�B
P�B
RTB
RoB
R�B
R�B
S&B
S@B
S@B
S@B
S@B
S@B
S@B
S@B
S&B
S@B
SuB
SuB
S�B
T,B
T�B
U�B
V�B
V�B
W
B
W?B
WYB
WsB
WsB
W�B
W�B
W�B
W�B
W�B
X_B
X�B
YB
YB
ZB
Y�B
Y�B
Y�B
YB
Z�B
[	B
[=B
[�B
\)B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\xB
]B
]/B
]/B
]dB
]dB
]dB
]~B
]dB
^B
^OB
^5B
^OB
^B
^B
^�B
^�B
^�B
`BB
`�B
`�B
`�B
`�B
`�B
`�B
a-B
a-B
a|B
aHB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
b4B
b�B
b�B
b�B
b�B
b�B
cB
c:B
cnB
c:B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dB
d@B
dZB
dZB
d@B
dZB
dZB
dZB
dtB
d�B
d�B
d�B
d�B
eB
eB
e,B
e,B
e,B
eFB
fB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
gB
g8B
gRB
gmB
g�B
g�B
g�B
g�B
h$B
h�B
h�B
h�B
hsB
hXB
h>B
h$B
h$B
h>B
hsB
hsB
h�B
h�B
i_B
i�B
i�B
j0B
jKB
jB
j�B
j�B
j�B
j�B
j�B
j�B
kB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
lqB
l�B
l�B
l�B
m)B
l�B
mwB
m�B
nIB
nIB
nIB
n�B
o B
oB
oB
o�B
o�B
poB
p�B
qB
p�B
q�B
q�B
r|B
r�B
sB
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
uB
u?B
u%B
u%B
uZB
uZB
utB
u�B
u�B
v+B
v`B
wB
wLB
w�B
w�B
w�B
w�B
xB
x�B
x�B
x�B
x�B
x�B
yXB
y�B
y�B
y�B
y�B
y�B
zDB
z*B
zDB
z*B
zDB
z*B
z*B
z�B
zxB
z�B
{B
{JB
{JB
{JB
{B
{B
{B
{�B
{�B
{�B
{�B
|�B
}�B
}�B
}�B
~B
~B
~B
~�B
~�B
~�B
~�B
~�B
~�B
cB
cB
}B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	5�B	5�B	5�B	5�B	5?B	5?B	5?B	4�B	5?B	5ZB	5ZB	5tB	9	B	=B	>�B	@ B	?�B	?�B	@OB	BB	E�B	J�B	L~B	R�B	Y�B	cTB	sB	��B	�)B	��B	�(B	��B
?B
�B�Ba�B}qB��B�qB�_B�3B�BȀB�IB�B�B�B�B!B1�B3B3MB4�B6`B6+B5%B4TB;dB6zB1�B(�B�B�BB�B�B�LB�dB�DB�BBw�BkB:DB
�B
��B
�=B
��B
u�B
^�B
O\B
0�B
�B
�B	�B	�*B	�B	��B	ҽB	ÖB	��B	��B	��B	�_B	�YB	��B	��B	�SB	��B	�EB	��B	��B	��B	��B	��B	��B	��B	�'B	��B	wB	shB	j0B	d�B	^5B	]�B	]�B	`'B	abB	_B	_pB	`�B	aB	`�B	_VB	bB	a�B	ZQB	\�B	_�B	bB	x�B	xB	uB	tnB	raB	�B	��B	��B	�[B	��B	��B	�)B	��B	��B	�4B	�}B	�SB	�XB	��B	��B	�FB	�LB	��B	�XB	�rB	�DB	�"B	ΥB	��B	�9B	�MB	ѷB	�DB	�]B	�]B	��B	�OB	��B	�B	�xB	�TB	��B	ٚB	�`B	�4B	�&B	ߤB	��B	�FB	�8B	�>B	��B	�CB	�/B	�/B	�IB	�/B	��B	��B	��B	��B	�B	�B	��B	�PB	��B	��B
�B
oB
�B
.B
�B
�B
�B	�wB	�	B	�zB	�B	�B	�B	�B	��B	��B	��B	��B	�tB	�B	�OB	�B	�B	�B	��B	�B	�B	�B	�B	�B	�DB	�QB	��B	�vB	��B	�!B	�B	�B	��B	��B	��B	�xB	��B	�$B	��B	��B	��B	�9B	�FB	�*B	��B	�HB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�2B	�B	�fB	�FB	��B	��B	�B	��B	��B	�B	�8B	�B	�B	��B	�2B	�LB	��B	��B	��B	��B	�RB	�lB	�lB	��B	�B	��B	�B	��B	�fB	�LB	��B	�8B	��B	�B	��B	��B	�(B	�PB	��B	��B	��B	��B	��B	��B	�rB	�rB	��B	�>B	��B	��B	�JB	��B	��B	��B	��B	�xB	�PB	��B	�B	�cB
 4B
�B
�B
B
�B
�B
�B
EB
1B
�B
�B
�B
_B
EB
�B
�B
B
�B
�B
tB
�B
�B
YB
?B
B
�B
+B
fB
	B
fB
�B
+B
�B
%B
B
�B
_B
B
1B
1B
�B
�B
B
�B
EB
�B
�B
�B
�B
B
�B
�B
+B
tB
�B
YB
�B
zB
	�B

XB

=B

	B

XB
B
�B
"B
<B
VB
VB
<B
<B
�B
�B
�B
B
B
B
�B
B
B
�B
B
PB
B
�B
pB
pB
pB
�B
vB
�B
�B
�B
�B
(B
BB
B
�B
�B
�B
�B
�B
�B
�B
B
NB
FB
?B
?B
sB
sB
�B
�B
+B
yB
_B
EB
EB
EB
EB
EB
EB
EB
EB
yB
B
EB
+B
+B
B
+B
EB
+B
EB
�B
yB
B
�B
�B
�B
�B
�B
WB
�B
kB
kB
�B
7B
QB
�B
QB
�B
�B
�B
�B
qB
�B
qB
�B
�B
B
�B
�B
�B
dB
IB
IB
B
~B
�B
�B
!B
B
B
;B
�B
!HB
 �B
 vB
!HB
!-B
!-B
!-B
!B
!-B
!-B
!B
!|B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$@B
$�B
$�B
%`B
%�B
&2B
&LB
&�B
&�B
&�B
'�B
(>B
(�B
(�B
)B
)*B
)_B
)�B
)�B
)�B
*0B
*�B
+B
+6B
+�B
+�B
+�B
,B
,"B
-]B
.B
./B
.cB
.�B
/B
0oB
1'B
1[B
1vB
1�B
2B
2GB
2aB
2aB
2�B
2�B
2�B
2�B
2�B
2�B
3hB
5B
5tB
5�B
5�B
5�B
6B
6FB
6FB
6�B
6�B
6�B
7�B
9$B
9rB
9rB
9�B
9�B
9�B
:DB
;dB
;0B
;�B
<B
<�B
=B
=B
="B
=qB
=�B
=�B
>(B
=�B
>�B
>�B
?�B
?}B
?}B
?}B
?cB
?�B
?�B
@�B
@�B
A�B
A�B
BuB
B�B
C{B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D3B
DMB
D�B
D�B
D�B
EB
EB
D�B
EB
EB
EmB
EmB
E�B
FB
F%B
FtB
FtB
F�B
F�B
GEB
G�B
G�B
H�B
H�B
I7B
IRB
IlB
IlB
I�B
I�B
I�B
J	B
I�B
I�B
I�B
I�B
J=B
KDB
KDB
K�B
LB
LJB
L�B
MB
MB
MB
MB
MjB
M�B
M�B
M�B
NB
N�B
N�B
OB
OBB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
PB
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
QhB
R�B
R�B
R�B
R�B
S@B
S@B
S@B
S@B
S@B
S@B
S@B
S@B
S[B
S[B
S�B
S�B
S�B
T�B
UgB
V9B
V�B
W
B
W$B
WYB
WsB
W�B
W�B
W�B
W�B
W�B
W�B
XB
XyB
X�B
YKB
Y�B
ZB
Y�B
Y�B
Y�B
Y�B
Z�B
[=B
[qB
[�B
\]B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]IB
]IB
]~B
]dB
]~B
]�B
]�B
^B
^jB
^5B
^OB
^5B
^OB
^�B
^�B
_;B
`vB
`�B
`�B
`�B
`�B
`�B
aB
a-B
aHB
a�B
abB
a�B
a�B
a�B
a�B
a�B
b4B
bB
b4B
b�B
cB
cB
b�B
cB
c B
c B
cnB
c�B
cnB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
d@B
dZB
dZB
d@B
dZB
dtB
dtB
d�B
d�B
d�B
d�B
d�B
e,B
e,B
eFB
e`B
ezB
e�B
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
gB
g8B
gRB
gmB
g�B
g�B
g�B
h
B
hXB
h�B
h�B
h�B
h�B
hXB
h>B
h$B
h>B
hXB
h�B
h�B
h�B
iB
iyB
i�B
jB
jKB
jeB
j�B
j�B
j�B
j�B
j�B
j�B
kB
kQB
l"B
k�B
k�B
k�B
k�B
k�B
lB
l=B
l�B
l�B
l�B
mB
mCB
mCB
m�B
nB
n}B
ncB
n�B
n�B
oB
o5B
oOB
o�B
p!B
p�B
qB
qAB
q'B
q�B
q�B
r�B
r�B
s3B
s�B
s�B
s�B
s�B
tB
tB
tnB
t�B
u%B
uB
u?B
u%B
u%B
uZB
utB
u�B
u�B
v+B
vFB
v�B
w2B
wfB
w�B
w�B
w�B
w�B
xRB
x�B
x�B
x�B
y	B
y	B
y�B
y�B
y�B
y�B
y�B
y�B
zDB
z*B
zDB
z*B
zDB
zDB
zDB
z�B
z�B
{B
{0B
{JB
{JB
{JB
{B
{B
{B
{�B
{�B
{�B
|B
|�B
}�B
}�B
}�B
~(B
~(B
~BB
~�B
~�B
~�B
~�B
~�B
~�B
�B
cB
�B
�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
</O<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201903040042272019030400422720190304004227202207271130042022072711300420220727113004202207271532532022072715325320220727153253  JA  ARFMdecpA30a                                                                20190523095839  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190523100015  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190523100016  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190523100016  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190523100016  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190523100016                      G�O�G�O�G�O�                JA  ARUP                                                                        20190523111514                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190222000000  CF  PSAL_ADJUSTED_QC@Q�@}p�G�O�                JM  ARCAJMQC2.0                                                                 20190303154227  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190303154227  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023004  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063253  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081507                      G�O�G�O�G�O�                