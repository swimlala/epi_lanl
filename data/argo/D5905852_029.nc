CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-11-19T09:38:59Z creation;2019-11-19T09:39:01Z conversion to V3.1;2022-08-02T05:11:48Z update;     
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
_FillValue                 �  ]P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a<   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20191119093859  20220818091504  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_029                    2C  D   APEX                            8420                            2.11.2                          846 @���*� 1   @���� @.�{����ch�s�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB&  B.ffB8ffBBffBFffBPffBW��B_��Bh  Bp  Bx  B�33B���B�  B�33B���B�  B���B�  B�ffB���B���B���B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  C 33C�3C�fC  C  C
  C  C  C�fC  C  C  C  C  C�C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ�CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AffAz�A<��A]�A}p�A���A��\A�z�A���AθRAޣ�A�\A��\BG�B=qB=qB��B%Q�B-�RB7��BA��BE��BO�RBVB^�RBg=qBoG�BwffB�
B�B�B��\B��fB�=qB��=B�G�B��=B�B�aHB�B�B��B��{B���B���B�k�B�z�BÞ�Bǳ3B˨�BϨ�BӨ�Bף�Bۨ�Bߨ�B㞸B瞸B랸B��B�qB���B��{B��C�C��C�=C��C	�\C�)C�{C�C�=C�\C�{C��C��C��C޸C��C!�{C#��C%�\C'�{C)��C+�=C-��C/�\C1��C3�{C5ٚC7�
C9��C;��C=��C?�=CA��CC�
CE�\CG�HCI��CK��CMCO��CQ�\CS��CU��CW�{CY��C[�\C]��C_�{Ca�{Cc��Ce�=Cg�=Ci��CkٚCm�
Co��Cq��Cs��Cu�
Cw�{Cy�=C{��C}�{C�\C���C��C��fC��fC��fC��=C��C��C��fC��C��=C��C��C��C��=C��C��fC��C��C���C��fC��C���C��C��=C��=C��=C��C��=C���C��=C��=C���C���C���C��=C���C��=C��=C��=C���C��=C��=C��C��=C��=C��=C��C��C��C��fC��C���C��fC��C��=C���C���C��fC��=C��C��C��C���C���C���C���C��=C��C��C���C��fC��fC��fC��C���C��=C��=C��=C��C���C��C��fC��C���C��fC���C��C��C��C���C��fC���C��C��C��=C���C���C��C��C���C��C��=C��C���C��C���C��C���C��=C��=C��C���C��C��C��C��C��fC���C���C��fC��C���C��C��C���C��C��D s�D ��Du�D��DuD�{Ds�D��Dt{D��Ds�D�3Ds3D��Dt{D��Ds3D�3D	uD	�{D
r�D
�Dr�D��DuD�3DqHD�3DvfD�
DuD�{DuD��Ds�D�Dr�D�Dq�D�Du�D��Dt{D�3DuD��Ds�D�3Du�D�{DuD�DuD�fDuD�3Ds�D��Ds�D��Dt{D��Du�D�3D s�D ��D!t{D!��D"s�D"�D#vfD#�{D$r�D$�3D%u�D%�fD&uD&��D's�D'�3D(t{D(�D)u�D)��D*u�D*��D+s3D+�D,s3D,�D-s3D-��D.s�D.�3D/s�D/�{D0s�D0��D1s�D1�{D2uD2�D3t{D3�D4u�D4��D5r�D5��D6s�D6��D7s3D7��D8q�D8��D9vfD9�
D:uD:�{D;s�D;�{D<vfD<��D=t{D=��D>s3D>��D?uD?�{D@t{D@�3DAr�DA�3DBs3DB�DCs�DC�3DDs�DD�{DEs�DE��DFt{DF�
DGxRDG�=DHu�DH��DIuDI�DJuDJ�{DKt{DK��DLs�DL��DMs�DM�3DNs3DN��DOuDO�{DPqHDP��DQt{DQ��DRvfDR��DSs�DS�DTs3DT�DUu�DU��DVs�DV�{DWuDW��DXs3DX��DYvfDY�DZt{DZ�D[uD[��D\u�D\�fD]s3D]�HD^q�D^��D_q�D_�{D`u�D`��Dau�Da�fDbvfDb�
DcvfDc�Ddu�Dd�DeuDe�{DfuDf�fDgt{Dg�DhuDh�DiuDi�Djt{Dj�{DkuDk�3Dls3Dl�{Dmr�Dm��DnvfDn��Dos�Do��Dpu�Dp�DquDq�DruDr�Dst{Ds��Dts�Dt�fDuuDu�{Dvs�Dv��Dws�Dw�{DxvfDx��Dys3Dy�HDzs�Dz�D{s3D{�D|u�D|��D}u�D}�
D~s�D~�Dt{D��D�9�D�yHD���D���D�:�D�z=D���D���D�:�D�z�D���D���D�9�D�z�D��3D��=D�:�D�{3D��=D���D�8�D�y�D��=D���D�:=D�y�D���D��=D�:=D�z�D���D���D�:=D�z=D��=D���D�9�D�yHD��HD���D�;3D�{3D���D���D�:�D�z=D���D���D�:=D�y�D���D���D�:�D�z�D���D���D�:�D�{3D��=D���D�:=D�{3D���D���D�:�D�{�D���D��=D�:=D�yHD���D��3D�:�D�z�D��=D���D�9�D�y�D���D��RD�8�D�y�D���D��3D�:�D�y�D��HD���D�9�D�yHD���D���D�9�D�y�D��HD��HD�9�D�z=D���D��3D�;�D�z�D���D���D�:�D�z�D��=D���D�9HD�yHD��3D��3D�:=D�{3D���D��3D�9�D�z=D���D���D�9�D�y�D���D��3D�:�D�z=D��=D���D�:=D�y�D���D��=D�9�D�z�D��3D��=D�9HD�y�D���D���D�:�D�z=D���D��=D�9�D�z=D���D���D�9HD�z=D��=D��=D�:�D�z�D���D���D�;3D�z�D���D���D�:=D�{3D��3D���D�8�D�y�D���D���D�:�D�y�D���D��3D�:�D�z�D���D���D�:�D�x�D���D���D�9�D�y�D��HD���D�:=D�y�D��=D���D�:=D�y�D���D���D�9�D�z�D��=D��=D�:�D�y�D���D��=D�:�D�z�D���D���D�;�D�z=D���D��HD�9�D�z�D���D��=D�;3D�z�D���D���D�9�D�y�D���D���D�:�D�y�D��=D��3D�;3D�z�D���D���D�:�D�{�D��=D���D�:=D�z�D��3D���D�9HD�z�D���D���D�:�D�{3D���D���D�9�D�z=D���D��HD�:�D�{3D���D��HD�9HD�z=D��3D��3D�9�D�x�D���D���D�:�D�z�D���D���D�9�D�y�Dº�D��=D�9HD�y�DùHD��=D�:�D�y�DĹ�D���D�9�D�z�DŻ3D���D�9�D�y�Dƹ�D���D�:�D�z=Dǹ�D���D�9�D�z�Dȹ�D��=D�;3D�{�Dɻ3D���D�9�D�z�Dʺ�D���D�9�D�z�D˺�D���D�9�D�z�D̻�D��=D�:�D�z�D͹�D���D�;3D�{3Dκ�D��3D�;3D�z=DϺ�D���D�9�D�y�Dй�D���D�;3D�y�DѺ=D���D�:=D�y�DҹHD���D�;�D�{�Dӻ�D���D�:�D�y�DԹ�D��HD�:=D�z�Dպ=D���D�9�D�y�DֹHD���D�:�D�z�D׺=D���D�;3D�{3Dغ�D���D�9�D�z=Dٺ�D���D�:�D�z�Dڻ3D���D�:�D�y�Dۺ�D���D�9�D�z=Dܺ�D���D�9HD�x�DݹHD���D�;3D�{3D޹�D���D�:�D�y�Dߺ�D��3D�9�D�y�D�=D��HD�:=D�y�D��D��=D�:�D�{3D�=D���D�:�D�z=D�=D���D�9�D�z=D�=D���D�9�D�y�D幚D���D�:=D�z=D�3D���D�:�D�z=D�=D���D�8�D�x�D�=D��3D�:�D�{3D麏D���D�:=D�yHD�HD��=D�9�D�x�D�HD���D�:=D�z�D�=D��HD�:=D�y�D��=D���D�:=D�z�D�=D���D�:�D�z=D��D��=D�:�D�y�D�HD���D�:=D�z�D��D��HD�8�D�z�D�D���D�9�D�z=D�3D���D�9�D�x�D���D��=D�:�D�y�D���D���D�8�D�y�D��RD��RD�8�D�y�D��=D���D�:�D�y�D���D���D�8�D�yHD��=D���D�:=D�{�D���D��3D�:�D�z�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A��A�� A��A�zA�A��A᳜A�A��AᵨA�tA᳜A�hA��A�UA�!AងA��A�~�AڶzA�p�Aر'A֠\A�4nA�zDA��,A�iDAɘ�A�OA�lWAƖ�A�چA�S�A�iyA�D�A���A�A�S�A���A�UgA��A���A��wA��3A���A���A�h�A��tA�$A�xA�X�A��BA��zA�[�A�OvA��>A��/A��FA�l�A��+A�0UA�A�A�rGA��|A��A���A�/�A�یA|
=Ay�4As�'An6AlE�Aie�Ad�LAa�A\�gAX�gAT5�AO�`AL�AI%AE��AC.IA?+kA;��A9xA7�A5^5A4��A46A2DgA/�wA.+�A,�hA,H�A,;A,�A/�A/6�A.��A.�tA-OvA-A*�rA'�A&2�A%�A%�
A%�mA%��A%[WA$�
A$n�A$)_A#�A#hsA#8�A!��A!�A e�A�A1'A�AE9AݘA�A�A��A�'A��Ap;AIRA�OA;dA�
AVmAϫAe�A#:A��A�A33A��A��A{�AOA�'AQ�AԕA�FAA�A�dA4A�A�vA�{A��A
�]A
�qA
��A	�A	�A��A2�A�A��A4AԕA�PAPHA4A��Au�A)�A��AxlAIRA \A��AQ�AѷAOvA�XA��AQA!A iA ��A ]d@��@��6@�|�@���@��N@�E9@��$@�@��X@�ߤ@�6�@���@�-w@��@�a@�T�@�F�@�C@�1'@��@��g@��Y@@���@�Ɇ@�2�@���@�=�@���@��@���@�k�@��@�-�@�G@��@��@���@�\�@��r@�`B@�-w@�-w@�/�@�e@�^@�_@��@�0U@���@��r@�X�@��"@��U@�[�@���@�B[@��@�$@�7�@�8�@��}@�J�@�m�@�"h@�m]@֋D@�J@�u�@�$t@ԥz@�{@Ӯ@�S�@��	@Ҍ�@�[�@��@��z@Ѫ�@�o�@��@���@К�@д9@�Ta@�o @��'@��@�)_@���@̇+@�S�@�L0@��@�8�@��8@���@�8�@���@�k�@�	l@��B@�-@�/@��X@ƕ�@�PH@�9X@���@ņ�@��@�&�@Â�@��@��@�@�$@�!@�ϫ@�Y@���@���@�P�@�@���@��@���@�~�@�a|@�7@�ԕ@���@�qv@��5@�~(@�(�@��:@�o@���@� �@��^@��P@�=@��6@��Z@�?}@���@��@�'�@��O@�1�@�	@��}@���@�e�@��@��j@���@�1�@���@�	@��#@��$@�;d@�!-@���@���@�c�@���@��'@��@��/@�|�@�Ft@��@���@�L�@�'�@�@��K@�ѷ@���@���@��m@���@��@��[@�6z@��@��}@�p;@�� @�=@��@���@�/�@���@�J�@�;@�w�@�)�@��r@���@��7@�Mj@��@���@�'R@��;@�S&@�"�@��@�͟@��6@��.@�|�@�l"@�E�@���@���@���@�zx@�S�@�:�@��@�֡@��@��b@���@�ff@�{@��K@��S@�O�@�@@�S@��5@��@���@���@��@�&�@���@���@�|�@�iD@�c�@�H�@��E@��@�U2@�@���@�>�@��K@���@�kQ@�6@�
�@���@�s�@�A�@��`@���@�V�@�
�@��m@���@�Y�@�H�@��@��K@���@���@���@���@��@���@�P�@��,@�M�@��Q@��@��0@��X@���@���@�B�@��1@�4n@��k@�X@�;d@���@��j@��@��\@�_@�;�@��@�N<@���@�m�@�0U@���@���@�y�@�a@�N<@�6z@��@�͟@��D@�*�@��&@���@���@�b�@�4@��@���@��r@�l"@�)�@���@��k@�e�@�Dg@�(�@�@@��f@���@�I�@��&@���@�n/@�,�@���@�u%@�l�@�Ov@�M@��;@��z@��@�O@�!�@��X@���@�}V@�V@�5?@��@�1@��m@�\�@��@���@���@�}V@�\�@�K^@�C-@�-@��@��}@��'@�s@�.I@��z@�q@�4@~��@~�@~�F@~��@~\�@~�@}�h@|��@|�4@|N�@{x@z�x@zZ�@y�Z@yY�@y&�@y�@x�|@x��@x?�@w��@w��@wqv@wMj@w i@v�'@vq�@v?@v4@u��@u��@uS&@u�@s�6@s4�@r�@rn�@r3�@q��@q�@q�"@qT�@q-w@q \@p�@p|�@p@o�0@oH�@nȴ@n�!@nR�@m��@m�@l�9@l6@k��@j��@i�@iQ�@i�@i�@h�@h9X@g��@g��@g�*@g$t@f�L@f�1@f3�@e��@e/@e%@d�[@dw�@d�@cn/@c�@b�c@b��@bTa@a��@ac�@a-w@a�@`�@`�o@`�@_��@_�@_S�@^�!@^@�@]�d@]x�@]=�@\ی@\�o@\Z@\Ft@\$@[�@[��@[s@[8@[�@Zȴ@Z��@Z��@Z�@Zv�@Y�@YN<@Y%F@X�K@X��@X�@W�@V�c@Vff@U@U�@T�?@T��@T9X@SdZ@R�@Q��@Q��@Q�@P��@P`�@P$@O�}@O8@NW�@NO@Mj@L�@Ly>@K�@Ko�@J�\@J
�@I��@I��@I�@H�@H?�@H�@G�@G�q@GF�@Fߤ@F�'@Fz@F@Eϫ@D��@Dc�@D�@C�*@C,�@C�@C i@B�@B�@BM�@A��@A�M@A+�@@��@@��@@�4@@�o@@@?�w@?Mj@>�2@>�<@>�1@>L0@=�'@=Vm@<��@<?�@;�g@;ƨ@;�*@;x@:��@:��@:h
@:�@9��@9[W@8֡@8�_@8A�@7��@7Y@6�@6��@6�A@6)�@5a�@5<6@4��@4��@4D�@4�@3�r@3��@3��@38@3(@2��@2\�@2�@1��@1�d@1<6@0ѷ@0��@0�9@0�@0�D@0z�@0e�@0-�@0�@/�m@/��@.ߤ@.L0@-�H@-c�@-<6@-�@,�O@,��@,m�@,]d@,6@+�]@+�w@+o�@+=@+�@*�@*O@)�@)�"@)?}@)+@)V@)�@(�@(��@(z�@(?�@'�K@'��@'J#@&��@&��@&��@&z@&	@%�#@%��@%x�@%[W@$��@$Ɇ@$�@$w�@$Ft@#�&@#�@#W?@#�@"�c@"��@"�r@"C�@"@!�@!�S@!��@!j@!=�@!!�@!�@ ��@ �U@ ��@ b@ݘ@��@�@a@A�@"�@��@�m@�b@��@Ov@)�@�)@�@N<@%F@!�@�@�u@I�@<�@4n@x@��@��@s@A�@�@@�,@E�@�Z@u�@!�@�@�@�f@�P@��@��@j@K^@�A@�F@��@\)@@O@o@�y@��@n�@!�@�@�3@k�@+�@ \@�@�@�P@�@��@"h@� @�@@�k@o�@)_@҉@��@{�@�@�@��@��@�X@m]@N<@2a@�@�E@�$@�4@tT@,=@1@�@�a@�[@�V@\)@(@�@�]@�s@��@s�@=q@($@e@{@�@�T@��@p�@a�@Y�@7L@�@��@��@q@c�@/�@1@�r@��@j�@H�@F�@>�@/�@,�@S@
��@
��@
��@
��@
YK@
+k@
&�@
$�@
@	�D@	�@	�S@	s�@	7L@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A��A�� A��A�zA�A��A᳜A�A��AᵨA�tA᳜A�hA��A�UA�!AងA��A�~�AڶzA�p�Aر'A֠\A�4nA�zDA��,A�iDAɘ�A�OA�lWAƖ�A�چA�S�A�iyA�D�A���A�A�S�A���A�UgA��A���A��wA��3A���A���A�h�A��tA�$A�xA�X�A��BA��zA�[�A�OvA��>A��/A��FA�l�A��+A�0UA�A�A�rGA��|A��A���A�/�A�یA|
=Ay�4As�'An6AlE�Aie�Ad�LAa�A\�gAX�gAT5�AO�`AL�AI%AE��AC.IA?+kA;��A9xA7�A5^5A4��A46A2DgA/�wA.+�A,�hA,H�A,;A,�A/�A/6�A.��A.�tA-OvA-A*�rA'�A&2�A%�A%�
A%�mA%��A%[WA$�
A$n�A$)_A#�A#hsA#8�A!��A!�A e�A�A1'A�AE9AݘA�A�A��A�'A��Ap;AIRA�OA;dA�
AVmAϫAe�A#:A��A�A33A��A��A{�AOA�'AQ�AԕA�FAA�A�dA4A�A�vA�{A��A
�]A
�qA
��A	�A	�A��A2�A�A��A4AԕA�PAPHA4A��Au�A)�A��AxlAIRA \A��AQ�AѷAOvA�XA��AQA!A iA ��A ]d@��@��6@�|�@���@��N@�E9@��$@�@��X@�ߤ@�6�@���@�-w@��@�a@�T�@�F�@�C@�1'@��@��g@��Y@@���@�Ɇ@�2�@���@�=�@���@��@���@�k�@��@�-�@�G@��@��@���@�\�@��r@�`B@�-w@�-w@�/�@�e@�^@�_@��@�0U@���@��r@�X�@��"@��U@�[�@���@�B[@��@�$@�7�@�8�@��}@�J�@�m�@�"h@�m]@֋D@�J@�u�@�$t@ԥz@�{@Ӯ@�S�@��	@Ҍ�@�[�@��@��z@Ѫ�@�o�@��@���@К�@д9@�Ta@�o @��'@��@�)_@���@̇+@�S�@�L0@��@�8�@��8@���@�8�@���@�k�@�	l@��B@�-@�/@��X@ƕ�@�PH@�9X@���@ņ�@��@�&�@Â�@��@��@�@�$@�!@�ϫ@�Y@���@���@�P�@�@���@��@���@�~�@�a|@�7@�ԕ@���@�qv@��5@�~(@�(�@��:@�o@���@� �@��^@��P@�=@��6@��Z@�?}@���@��@�'�@��O@�1�@�	@��}@���@�e�@��@��j@���@�1�@���@�	@��#@��$@�;d@�!-@���@���@�c�@���@��'@��@��/@�|�@�Ft@��@���@�L�@�'�@�@��K@�ѷ@���@���@��m@���@��@��[@�6z@��@��}@�p;@�� @�=@��@���@�/�@���@�J�@�;@�w�@�)�@��r@���@��7@�Mj@��@���@�'R@��;@�S&@�"�@��@�͟@��6@��.@�|�@�l"@�E�@���@���@���@�zx@�S�@�:�@��@�֡@��@��b@���@�ff@�{@��K@��S@�O�@�@@�S@��5@��@���@���@��@�&�@���@���@�|�@�iD@�c�@�H�@��E@��@�U2@�@���@�>�@��K@���@�kQ@�6@�
�@���@�s�@�A�@��`@���@�V�@�
�@��m@���@�Y�@�H�@��@��K@���@���@���@���@��@���@�P�@��,@�M�@��Q@��@��0@��X@���@���@�B�@��1@�4n@��k@�X@�;d@���@��j@��@��\@�_@�;�@��@�N<@���@�m�@�0U@���@���@�y�@�a@�N<@�6z@��@�͟@��D@�*�@��&@���@���@�b�@�4@��@���@��r@�l"@�)�@���@��k@�e�@�Dg@�(�@�@@��f@���@�I�@��&@���@�n/@�,�@���@�u%@�l�@�Ov@�M@��;@��z@��@�O@�!�@��X@���@�}V@�V@�5?@��@�1@��m@�\�@��@���@���@�}V@�\�@�K^@�C-@�-@��@��}@��'@�s@�.I@��z@�q@�4@~��@~�@~�F@~��@~\�@~�@}�h@|��@|�4@|N�@{x@z�x@zZ�@y�Z@yY�@y&�@y�@x�|@x��@x?�@w��@w��@wqv@wMj@w i@v�'@vq�@v?@v4@u��@u��@uS&@u�@s�6@s4�@r�@rn�@r3�@q��@q�@q�"@qT�@q-w@q \@p�@p|�@p@o�0@oH�@nȴ@n�!@nR�@m��@m�@l�9@l6@k��@j��@i�@iQ�@i�@i�@h�@h9X@g��@g��@g�*@g$t@f�L@f�1@f3�@e��@e/@e%@d�[@dw�@d�@cn/@c�@b�c@b��@bTa@a��@ac�@a-w@a�@`�@`�o@`�@_��@_�@_S�@^�!@^@�@]�d@]x�@]=�@\ی@\�o@\Z@\Ft@\$@[�@[��@[s@[8@[�@Zȴ@Z��@Z��@Z�@Zv�@Y�@YN<@Y%F@X�K@X��@X�@W�@V�c@Vff@U@U�@T�?@T��@T9X@SdZ@R�@Q��@Q��@Q�@P��@P`�@P$@O�}@O8@NW�@NO@Mj@L�@Ly>@K�@Ko�@J�\@J
�@I��@I��@I�@H�@H?�@H�@G�@G�q@GF�@Fߤ@F�'@Fz@F@Eϫ@D��@Dc�@D�@C�*@C,�@C�@C i@B�@B�@BM�@A��@A�M@A+�@@��@@��@@�4@@�o@@@?�w@?Mj@>�2@>�<@>�1@>L0@=�'@=Vm@<��@<?�@;�g@;ƨ@;�*@;x@:��@:��@:h
@:�@9��@9[W@8֡@8�_@8A�@7��@7Y@6�@6��@6�A@6)�@5a�@5<6@4��@4��@4D�@4�@3�r@3��@3��@38@3(@2��@2\�@2�@1��@1�d@1<6@0ѷ@0��@0�9@0�@0�D@0z�@0e�@0-�@0�@/�m@/��@.ߤ@.L0@-�H@-c�@-<6@-�@,�O@,��@,m�@,]d@,6@+�]@+�w@+o�@+=@+�@*�@*O@)�@)�"@)?}@)+@)V@)�@(�@(��@(z�@(?�@'�K@'��@'J#@&��@&��@&��@&z@&	@%�#@%��@%x�@%[W@$��@$Ɇ@$�@$w�@$Ft@#�&@#�@#W?@#�@"�c@"��@"�r@"C�@"@!�@!�S@!��@!j@!=�@!!�@!�@ ��@ �U@ ��@ b@ݘ@��@�@a@A�@"�@��@�m@�b@��@Ov@)�@�)@�@N<@%F@!�@�@�u@I�@<�@4n@x@��@��@s@A�@�@@�,@E�@�Z@u�@!�@�@�@�f@�P@��@��@j@K^@�A@�F@��@\)@@O@o@�y@��@n�@!�@�@�3@k�@+�@ \@�@�@�P@�@��@"h@� @�@@�k@o�@)_@҉@��@{�@�@�@��@��@�X@m]@N<@2a@�@�E@�$@�4@tT@,=@1@�@�a@�[@�V@\)@(@�@�]@�s@��@s�@=q@($@e@{@�@�T@��@p�@a�@Y�@7L@�@��@��@q@c�@/�@1@�r@��@j�@H�@F�@>�@/�@,�@S@
��@
��@
��@
��@
YK@
+k@
&�@
$�@
@	�D@	�@	�S@	s�@	7L@	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B1B�B�B	�BxB�B�BBoB@BuB�B,B,BaBFBB�B�B�B	��B	��B	��B	��B	��B	�dB	��B	�B	��B	��B	��B	�]B	�UB	ªB	�5B	��B	�QB	�B
BB
K�B
��B
��B
�hB
�hBV�BMPB/�B*eB#:B 'B7B$BBDB9B
�	B
��B
յB
��B
�KB
��B
�9B
A B
M�B
@�B
/�B
SB
�B	��B	�tB	�B	��B	�B	n}B	dZB	PbB	5�B	 vB	JB�B�&BՁB�rB��B�LB��B�
B�B�$B�,B��B��B��B�6B�]B�vB��B�3B�qBбB	$�B	EB	V�B	gB	l�B	q�B	h�B	T�B	L�B	U�B	_pB	h$B	tnB	�B	�B	~�B	~�B	�iB	�;B	��B	�B	�<B	��B	��B	�[B	�B	�-B	�NB	�fB	��B	��B	�2B	��B	��B	��B	��B	�DB	�B	�wB	�vB	��B	��B	�B	�FB	�lB	�DB	��B	�xB	�xB	�^B	�B	��B	�dB	��B	�6B	�<B	�qB	��B	�qB	�BB	�HB	�B	�'B	��B	�{B	ĜB	�B	��B	��B	��B	�7B	��B	�XB	�rB	��B	��B	�)B	ˬB	�B	͹B	�jB	͹B	͟B	��B	�B	��B	бB	��B	уB	��B	ЗB	ѝB	ҽB	��B	�uB	��B	��B	�@B	�uB	��B	�,B	�FB	��B	��B	�aB	��B	�2B	�2B	�B	��B	�{B	��B	��B	�2B	�gB	ԯB	յB	ՁB	��B	�mB	רB	��B	�1B	خB	��B	�EB	��B	��B	��B	�+B	�B	�sB	��B	�
B	׍B	��B	׍B	�yB	�+B	ּB	�gB	��B	ՁB	�{B	�,B	�aB	��B	��B	��B	�B	��B	��B	�B	�B	�B	��B	�QB	�7B	ںB	ںB	��B	�WB	یB	��B	�]B	�]B	�]B	ܬB	��B	�/B	�/B	��B	�OB	��B	�\B	ߤB	�bB	�4B	�NB	�-B	�B	�B	�B	�B	��B	�TB	��B	�B	�mB	�B	�RB	�B	��B	�B	�B	�B	��B	�sB	�sB	�B	��B	�B	�*B	�B	��B	�0B	�B	�B	�qB	�IB	�B	�B	��B	�B	�!B	��B	�B	�B	��B	�B	�tB	��B	�B	�fB	�xB	�^B	�B	��B	�B	�B	��B	�B	�PB	��B	��B	��B	��B	�B	��B	�$B	��B	�^B	�<B	�wB	�B
  B
 B
  B
  B	��B	��B
 B
�B
�B
AB
[B
�B
aB
GB
�B
�B
�B
�B
�B
9B
B
�B
+B
B
YB
YB
�B
�B
�B
�B
�B
�B
�B
�B
	B
	�B

�B

�B

�B
DB
�B
�B
�B
�B
0B
0B
JB
dB
�B
�B
�B
B
6B
�B
B
VB
�B
BB
pB
(B
�B
�B
.B
.B
B
�B
�B
�B
4B
4B
 B
[B
[B
uB
�B
�B
�B
�B
�B
B
�B
�B
B
B
�B
�B
@B
B
�B
[B
uB
�B
�B
�B
�B
:B
�B
:B
 B
B
@B
,B
�B
gB
�B
�B
$B
?B
�B
�B
B
B
�B
�B
�B
�B
#B
�B
�B
�B
�B
/B
�B
�B
�B
;B
VB
!B
�B
!-B
!-B
!HB
!HB
!�B
!�B
!�B
"B
"hB
"�B
#�B
#TB
#�B
#�B
#�B
$tB
$�B
%FB
%B
$�B
%FB
%�B
%�B
%�B
&2B
&�B
'8B
'RB
'�B
'RB
'�B
'�B
'�B
($B
(�B
)B
)_B
)�B
)�B
*B
*eB
*B
*B
*�B
*�B
*�B
+B
+B
+QB
+QB
+kB
+�B
,�B
-]B
-�B
.B
/�B
/�B
/�B
/�B
0;B
0�B
0�B
0�B
0�B
1�B
1�B
3B
3�B
3�B
49B
49B
49B
4B
49B
4�B
5B
5?B
5?B
5ZB
5�B
5�B
5�B
5�B
6B
5�B
5tB
5?B
5tB
6FB
6B
5�B
5�B
6`B
6�B
72B
7fB
7�B
8lB
8�B
8�B
8�B
9�B
9XB
9>B
9�B
9�B
:DB
:DB
:DB
:^B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:DB
:DB
:�B
:�B
;JB
;0B
;0B
;�B
;B
<B
<B
<6B
<6B
<jB
<jB
<�B
<jB
<jB
<�B
<�B
<�B
<�B
=<B
="B
=B
=B
=�B
>(B
>(B
>]B
>�B
?cB
?.B
?.B
?.B
?B
?cB
?�B
@B
@ B
@ B
@OB
@�B
@�B
A B
AoB
A�B
A�B
BB
BAB
B[B
B�B
CB
C-B
CB
C{B
C�B
DB
DB
DB
DB
D�B
D�B
D�B
D�B
EB
EmB
E�B
F?B
FYB
F�B
F�B
F�B
F�B
GB
GB
GEB
G+B
G_B
G_B
G�B
G�B
G�B
G�B
G�B
G�B
HfB
H�B
H�B
H�B
IB
IRB
I�B
I�B
J=B
JXB
J�B
KB
J�B
KB
K^B
K�B
LdB
L�B
L�B
M6B
M6B
MPB
MjB
M�B
N<B
N"B
N�B
OB
O(B
OBB
O�B
P.B
P}B
PbB
PbB
QB
QNB
Q�B
Q�B
Q�B
Q�B
R B
RTB
R:B
RoB
R�B
R�B
S[B
S�B
S�B
TaB
T�B
T�B
T�B
T�B
T�B
U2B
UMB
U�B
U�B
VB
VmB
W�B
W�B
W�B
X+B
X�B
YB
YKB
YKB
YB
Z7B
ZkB
Z�B
[�B
[�B
[�B
[�B
[�B
\CB
\xB
\]B
\xB
\�B
\�B
]B
]/B
]IB
]/B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
^jB
_VB
_;B
_VB
_�B
_�B
_�B
`'B
`\B
`�B
`�B
`�B
`�B
`�B
a-B
aHB
aHB
abB
aHB
abB
abB
a|B
a�B
a�B
a�B
a�B
b�B
cB
cTB
cTB
cnB
c�B
dB
d&B
d@B
d@B
dtB
d�B
d�B
eFB
eFB
eFB
ezB
e�B
fB
ffB
f�B
f�B
f�B
f�B
f�B
gB
gB
gRB
g�B
g�B
h
B
hsB
hsB
hsB
h�B
iB
i*B
iyB
i�B
i�B
jB
jB
jB
jeB
jB
j�B
kB
k6B
kkB
k�B
k�B
k�B
l"B
lWB
l�B
l�B
l�B
l�B
l�B
mB
m)B
mB
mCB
mwB
m�B
n/B
nIB
n}B
n�B
n�B
o B
o5B
o5B
o5B
oiB
o�B
pB
p!B
p;B
p�B
qB
q�B
q�B
q�B
q�B
r�B
sB
sMB
shB
s�B
shB
shB
shB
sMB
s�B
sMB
s�B
tB
t�B
t�B
u�B
u�B
u�B
vB
vFB
v`B
vFB
v`B
v`B
v`B
vzB
vzB
v�B
v�B
v`B
u�B
u�B
u�B
v�B
w2B
wB
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
x8B
x8B
xRB
xlB
y	B
y>B
yrB
y�B
y�B
y�B
y�B
y�B
zDB
z^B
z�B
z�B
z�B
z�B
z�B
{B
{JB
{dB
{�B
{�B
{�B
|B
|B
|jB
|�B
|�B
|�B
|�B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
~]B
~�B
~�B
B
HB
HB
HB
cB
�B
�B
�B
�B
�B
� B
�4B
�4B
�iB
�iB
�OB
��B
��B
�B
��B
��B
�'B
��B
��B
��B
��B
�B
��B
�'B
�'B
�[B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B1B�B�B	�B�B�B�B.BoB@BuB�BFBFBaB{B�BqB~B%B	�SB	�5B	��B	�XB	�B	�[B	��B	�gB	��B	��B	�RB	��B	�RB	�)B	��B	��B	��B
'B
mB
O�B
�:B
��B
�LB
��BZ�BP}B0�B+�B%�B#�B5B�B_BBKB
��B
�B
��B
�jB
�VB
�B
��B
F�B
R B
EB
5tB
QB
	�B	�B	̳B	��B	��B	��B	q�B	h�B	VB	:�B	%�B	�B��B�B�B��B��B�JB��B�WB�eB�eB��B��B�RB��B�/B��B��B�nB��B��B��B	$�B	E�B	W�B	h�B	m�B	t�B	l�B	VSB	M6B	VB	_�B	h�B	u%B	��B	�iB	cB	�B	��B	�B	��B	�<B	�(B	��B	�HB	��B	�B	�B	��B	��B	�8B	�RB	��B	��B	�KB	�kB	�KB	��B	�QB	�IB	�-B	��B	��B	�%B	�2B	��B	��B	��B	��B	�dB	��B	�B	�qB	�6B	�qB	�VB	�B	��B	�B	�BB	�cB	�iB	B	��B	�B	āB	�9B	��B	�_B	�zB	ȀB	ɺB	�XB	��B	��B	�^B	�xB	˒B	�0B	͹B	�"B	��B	�pB	�VB	ΥB	��B	ϑB	�4B	�:B	��B	�NB	�B	�TB	ӏB	��B	�,B	ӏB	өB	��B	��B	�FB	ԯB	��B	�MB	�gB	�B	ՁB	յB	�gB	�MB	�2B	�MB	��B	��B	�B	�SB	��B	ևB	��B	�9B	��B	�B	�eB	ٴB	�B	�yB	��B	�EB	�+B	�EB	خB	ؓB	��B	�YB	�$B	��B	�B	�+B	�KB	�1B	��B	�9B	��B	�B	��B	ԕB	ԯB	՛B	��B	�YB	�EB	�B	��B	�QB	�kB	چB	چB	ڠB	��B	�qB	�=B	�WB	��B	�B	�xB	��B	��B	��B	�B	�/B	�~B	�~B	�B	ޞB	�'B	�B	��B	�|B	�B	�B	��B	�4B	�B	��B	�B	�:B	�B	�zB	�mB	�B	��B	��B	��B	�8B	�$B	�
B	�$B	�B	��B	��B	��B	��B	�B	�B	�B	�QB	�B	�B	�WB	�B	�B	��B	��B	��B	��B	��B	�AB	�GB	��B	�B	��B	��B	��B	�`B	��B	��B	��B	��B	�B	��B	�B	�6B	��B	��B	�PB	�B	�B	�VB	��B	�B	��B	��B	�xB	��B	��B	�cB
 OB
 iB
 OB
 OB
 iB
 iB
�B
AB
'B
�B
�B
�B
{B
�B
3B
MB
MB
�B
B
�B
mB
�B
zB
zB
�B
�B
+B
�B
B
+B
zB
�B
�B
	B
	lB

=B

�B

�B
^B
�B
JB
0B
�B
~B
�B
~B
�B
�B
6B
B
6B
PB
�B
VB
pB
�B
B
�B
�B
\B
�B
�B
bB
bB
HB
HB
HB
 B
hB
hB
:B
�B
�B
�B
�B
�B
B
,B
FB
aB
�B
�B
aB
FB
B
�B
uB
&B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
oB
[B
�B
�B
�B
�B
9B
�B
sB
�B
EB
�B
�B
kB
�B
�B
�B
�B
qB
�B
�B
B
/B
dB
�B
5B
�B
�B
�B
�B
 \B
!�B
!bB
!bB
!|B
!�B
!�B
"B
"�B
"�B
#TB
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%zB
%zB
%�B
%�B
&B
&LB
&fB
&�B
'mB
'�B
'�B
'�B
'�B
'�B
($B
(sB
(�B
)DB
)�B
)�B
*B
*eB
*�B
*�B
*�B
+B
+B
+B
+6B
+QB
+�B
+�B
+�B
,"B
-B
-�B
-�B
.}B
0B
0!B
/�B
0B
0�B
0�B
0�B
0�B
1B
2B
2B
3hB
4B
4B
4nB
4nB
4TB
4nB
4�B
5?B
5ZB
5tB
5�B
5�B
5�B
5�B
5�B
6B
6FB
6B
5�B
5�B
5�B
6�B
6zB
6zB
5�B
6zB
7B
7fB
7�B
8B
8�B
9>B
9>B
9>B
9�B
9rB
9�B
9�B
:B
:xB
:xB
:xB
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:xB
:xB
:�B
:�B
;�B
;B
;�B
<B
;�B
<6B
<6B
<PB
<jB
<�B
<�B
<�B
<�B
<�B
<�B
=B
="B
="B
=�B
=<B
=VB
=qB
>B
>wB
>wB
>�B
?.B
?�B
?}B
?cB
?cB
?cB
?�B
@ B
@OB
@B
@OB
@�B
@�B
@�B
AoB
A�B
B'B
A�B
B[B
B�B
B�B
C-B
CGB
CaB
CaB
C�B
C�B
DMB
DMB
DMB
DgB
D�B
D�B
D�B
EB
E9B
E�B
F%B
FtB
FtB
F�B
F�B
G+B
G+B
G+B
GEB
GzB
G_B
G�B
GzB
G�B
G�B
G�B
HB
HB
HKB
H�B
H�B
H�B
IB
IRB
I�B
I�B
J	B
J�B
J�B
K)B
KDB
K)B
KxB
K�B
LB
L�B
M6B
MB
MjB
MjB
M�B
M�B
NVB
NpB
N�B
O(B
OBB
OvB
O�B
PB
P}B
P�B
P�B
P�B
QhB
QhB
Q�B
Q�B
Q�B
RB
RTB
R�B
RoB
R�B
R�B
SB
S�B
S�B
TB
T�B
T�B
T�B
T�B
T�B
T�B
UgB
U�B
U�B
VB
V9B
V�B
W�B
W�B
W�B
XyB
X�B
YKB
YB
Y�B
Y�B
ZkB
Z�B
[#B
[�B
[�B
[�B
\B
\CB
\]B
\�B
\�B
\�B
\�B
]/B
]IB
]~B
]�B
]~B
]/B
]B
\�B
\�B
]B
^B
^B
^�B
_�B
_pB
_�B
_�B
_�B
`'B
`\B
`�B
`�B
`�B
aB
aB
a-B
abB
abB
abB
a|B
a|B
a|B
a�B
a�B
a�B
a�B
bB
bNB
cB
cTB
c�B
cnB
c�B
c�B
d&B
dZB
dZB
dtB
d�B
d�B
e,B
ezB
e`B
e�B
e�B
f2B
fLB
f�B
f�B
f�B
f�B
f�B
gB
g8B
gRB
g�B
g�B
h$B
h>B
h�B
h�B
h�B
h�B
iDB
i_B
i�B
i�B
i�B
jKB
jKB
j0B
jB
j�B
kB
k6B
kkB
k�B
k�B
lB
lB
lWB
l�B
l�B
l�B
l�B
mB
m)B
mCB
mCB
mCB
mwB
m�B
n/B
ncB
n}B
n�B
n�B
o B
o5B
oiB
oOB
oiB
o�B
pB
p!B
pUB
pUB
p�B
q'B
q�B
q�B
q�B
q�B
r�B
sMB
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t9B
t�B
t�B
u�B
u�B
u�B
vFB
vzB
v�B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
vzB
v+B
vB
v+B
v�B
wLB
w2B
v�B
v�B
v�B
v�B
v�B
w2B
w�B
xB
xRB
xlB
x�B
x�B
y$B
yXB
y�B
y�B
y�B
zB
zB
y�B
zxB
zxB
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
|B
|6B
|PB
|�B
|�B
|�B
|�B
|�B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
~(B
~wB
~�B
B
HB
}B
}B
cB
}B
� B
�B
� B
�B
� B
�4B
�OB
�4B
��B
��B
��B
��B
��B
�;B
��B
��B
�[B
�B
�B
�B
�'B
�AB
�'B
�[B
�AB
�uB
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<'�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.18(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201911300042332019113000423320191130004233202207271133382022072711333820220727113338202207271536072022072715360720220727153607  JA  ARFMdecpA30a                                                                20191119093749  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191119093859  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191119093859  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191119093900  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191119093900  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191119093900  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191119093901                      G�O�G�O�G�O�                JA  ARUP                                                                        20191119095500                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191129154233  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191129154233  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023338  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063607  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091504                      G�O�G�O�G�O�                