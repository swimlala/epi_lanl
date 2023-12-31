CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-12-15T15:39:42Z creation;2020-12-15T15:39:45Z conversion to V3.1;2022-08-17T01:55:27Z update;     
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
_FillValue                 �  ]d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aT   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ߬   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20201215153942  20220818091506  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               DA   JA  A30_8420_068                    2C  D   APEX                            8420                            2.11.2                          846 @�O�2� 1   @�O"�9�@/Y��|��c����1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0��B8ffB>��BH��BO��BW33B`ffBg��Bp  Bx  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  C   C  C  C  C  C
  CL�C��C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:33C<  C>  C@  CB�CD  CE�fCG�fCJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ Dݼ�D�  D�@ Dހ D��3D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @\)@w
=@��H@��AG�A=��A^{A}��A�
=A�p�A�33A��RAθRA���A�
=A��HBz�BffBQ�Bp�B'�B0p�B7��B>ffBH=qBO{BVz�B_�Bg(�BoG�Bw=qB��B��)B���B���B���B��3B��RB�B��3B���B��qB���B���B��
B�B���B���B��Bǔ{B˨�BϮBӳ3B׳3B۽qB߽qB�ǮB��
B���B�ǮB�{B���B��B��3C�)C�HC�fC�HC	�fC5�C�C��C޸C޸C�)C�)C�)C�HC޸C��C!�fC#ٚC%ٚC'�
C)�)C+޸C-ٚC/�
C1ٚC3޸C5�HC7��C:�C;�
C=޸C?�HCA��CC�)CE�=CG��CI�
CK�)CMٚCO�{CQ�{CS�)CU��CW�)CY�
C[�)C]�HC_�HCa�
Cc�
Ce�)Cg�{Ci��Ck�{Cm�\CoٚCq��Cs�HCu޸Cw�HCy�fC{޸C}޸C�HC���C��C��C��C���C��C��\C��C���C��C��C���C��=C��=C��C��=C��3C��3C���C���C���C��C��C��\C��C��\C��C��C���C��C��C���C��C���C��C��C��C��C��C���C��=C��C��C��C��=C���C��C��C���C��C��\C��\C���C���C��=C���C���C���C���C��C��C��\C���C��3C��{C���C��C��=C��C���C���C��C��\C��\C���C��\C��C���C��\C��C��\C��\C��C��C��C��C���C��C��C��C���C��C��)C���C���C��=C��C���C���C��C��\C��C���C��C��C��C��C���C��C��C��C��C���C��\C��C��C��C��\C��C��=C���C��C��C��C��\C���C���C��D uD ��Dw
D��DvfD�fDvfD��Dx�D�
DuD�fDw�D�RDw
D�
Dw�D�fD	xRD	��D
vfD
�
Dw�D�RDw�D�
Dw�D�fDw�D��Dx�D��DvfD��Dw
D�fDuD�{Dw�D��DvfD��Dw
D�RDx�D��Dx�D�RDu�D�Dw�D��Dw�D�fDuD��DvfD�fDuD��Dw
D��Dw�D�D u�D �RD!x�D!�fD"uD"�fD#vfD#�=D$|)D$�
D%w
D%�fD&w
D&�
D'vfD'�
D(w�D(��D)xRD)��D*w�D*�fD+w�D+��D,w�D,��D-w
D-��D.w
D.��D/vfD/�fD0w
D0�D1w
D1��D2vfD2�D3w�D3�fD4w
D4��D5xRD5�
D6s�D6�D7xRD7�RD8xRD8��D9w�D9�fD:uD:��D;xRD;�D<w
D<�
D=w
D=��D>w
D>�RD?x�D?��D@u�D@�DAt{DA�
DBvfDB�DCuDC�DDu�DD��DEx�DE�
DFt{DF�DGuDG��DHu�DH��DIw�DI��DJu�DJ�DKuDK�{DLt{DL��DMw
DM�fDNx�DN��DOw�DO��DPxRDP��DQz�DQ�=DRxRDR��DSvfDS��DTw�DT��DUw�DU�
DVw
DV��DWu�DW�
DXx�DX�
DYw
DY�
DZu�DZ�
D[xRD[�
D\w�D\�RD]w�D]�D^uD^��D_w
D_��D`vfD`�fDavfDa��Dbw
Db�{DcvfDc�fDdw�Dd��Dew
De�Dfu�Df�
Dgx�Dg��Dhx�Dh�
Diu�Di�
Dju�Dj��Dku�Dk��Dlw�Dl��Dmw�Dm�
Dnw
Dn��DovfDo�fDpvfDp�{Dqs3Dq��Drs�Dr�Dsw�Ds�RDtu�Dt�
DuxRDu�
Dvw
Dv�RDwy�Dw��DxuDx��DyvfDy�Dzw�Dz�RD{x�D{��D|w
D|�{D}vfD}�
D~w�D~��DvfD��D�<{D�|)D��{D��)D�;�D�{�D��3D���D�;�D�{�D��3D��3D�<)D�{�D���D���D�<)D�{�D���D��{D�;�D�|{D��)D���D�;3D�{�D���D���D�;�D�{3D��3D���D�;�D�z�D���D���D�;�D�{�D���D���D�:�D�{�D��{D��)D�;3D�{�D���D���D�:�D�|)D���D��3D�:=D�z�D��)D���D�<{D�|)D���D��)D�<{D�|)D���D���D�:�D�{3D���D��3D�:�D�z�D���D���D�;3D�z�D���D��=D�:�D�{3D���D���D�<�D�{�D���D��)D�;�D�|{D��{D���D�:�D�{3D���D��{D�;�D�{3D��{D��D�<)D�{�D���D���D�;�D�{�D���D���D�:�D�{3D���D��3D�;3D�z�D���D���D�:�D�{�D���D��3D�;3D�z�D���D��=D�:=D�z�D��=D���D�;�D�{�D��3D��3D�;�D�{�D���D��3D�;�D�{�D���D��)D�;�D�{�D��{D���D�;�D�{�D��)D���D�;�D�|)D��)D���D�;3D�y�D���D���D�:�D�{�D��{D���D�<)D�{3D���D���D�<)D�{�D���D���D�;�D�{�D���D���D�<�D�|)D��{D���D�:=D�z�D��)D���D�;�D�{�D���D��)D�;�D�|)D��{D��D�<)D�{�D���D���D�<)D�{�D���D��3D�;�D�{�D���D���D�;3D�z�D���D���D�;�D�{�D���D���D�:�D�{�D���D���D�;3D�{�D���D���D�;�D�{�D��{D���D�<)D�{�D���D��3D�:�D�z�D���D��{D�<{D�|)D��)D���D�;�D�{�D��)D��D�<�D�{�D��3D��3D�;3D�|)D���D��3D�:�D�{3D��)D���D�;3D�{3D���D���D�<{D�{�D���D���D�;�D�{�D���D���D�<)D�|)D���D��3D�:�D�z�D���D���D�;3D�{3D»3D���D�;�D�{�Dü)D��)D�;�D�}Dļ)D��3D�;�D�|)DŻ�D���D�;�D�{3Dƺ�D���D�;3D�{3DǼ)D���D�:�D�{3DȻ�D���D�;�D�{�Dɻ3D��3D�:�D�z�Dʻ3D���D�:�D�z�D˺�D���D�;�D�z=D̻�D���D�;�D�z�Dͺ=D���D�;3D�|)Dμ�D���D�;�D�{�Dϼ�D��3D�:=D�z�Dк�D���D�;�D�z�DѺ�D��)D�<)D�{�Dһ�D��)D�;�D�z�DӼ)D���D�;�D�|�DԼ)D���D�<)D�|�Dջ�D��3D�<)D�{�Dֺ�D��)D�<{D�z�D׻�D��)D�:�D�z=Dغ�D��)D�;�D�{�Dٻ3D���D�;�D�{�Dڹ�D��3D�<�D�|)Dۻ�D��3D�;�D�|�Dܼ)D���D�:�D�y�Dݹ�D���D�;�D�|)D޽qD���D�<�D�|{D߻�D���D�;�D�{�D�)D��{D�:�D�z�DẏD���D�<)D�{�D⺏D��3D�;�D�{�D�D���D�;�D�{�D��D���D�;3D�{3D�)D���D�;3D�{�D滅D���D�<)D�|{D�)D���D�;�D�{�D��D��3D�:�D�{3D��D���D�:�D�{3D�3D���D�;3D�z�D뻅D��)D�;�D�|�D�)D���D�:�D�{�D��3D��3D�:�D�{3D�)D��{D�;�D�{3D��D���D�;3D�z�D��D���D�:�D�{3D�)D���D�:�D�z�D��D���D�:=D�{�D��D��3D�;3D�{�D���D��D�<)D�{3D��)D��)D�:�D�{3D��{D��)D�<{D�{3D���D��{D�=D�|�D��D���D�:=D�{3D��3D���D�;�D�{�D��3D���D�:=D�eq1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aϩ_AϫAϬqAϮ�AϰUAϵAϸ�Aϸ�AϹXAϺ*AϺ�Aϻ�A�� A�ĜA��tA�ŢA���A��?A���A��9A�ʌA��#A���AϽ�A�4nAǗ�AǺ*A�z�A�@OA�XyA�/OA� \A�
�A�)�A���A�ZQA�@A���A�oA��zA�tA���A�0!A�oA��0A�یA��oA��@A��A�ŢA� �A��OA�I�A��A��IA�[�A�~�A�� A�!-A���A���A��RA��?A��FA�aA�!-Aw�As�PAl�Ad�#AbA�A^��AY��AT��AR'�AOqAN��AL&�AIAG��AG>BAF��AB��A?{A<��A;j�A:�A9iDA8�A5��A4y�A2a�A1K^A1'RA0�A0e,A.��A-��A*J�A'g�A%�6A&�A&zA'DgA(�"A*��A,�{A-�[A.=�A.�A/�A/qA/$tA/=A/*�A/eA.�uA.J�A.�A.�yA.ߤA.�5A.�oA.��A.iDA-�0A,�!A+��A+A*��A)}VA(��A'��A'[WA'!-A&��A%�A%b�A$>BA#��A#�A"�A"X�A!��A �A��A��A��A\�A%�A��AXyA�9A/�A�5At�Av�A��A4A-�AݘA�@A� A�dA�A-wAp�A�$A4A�ZA'�A�A��A��A�AxlADgA�A��A1�A�A{JA"�A�UAYA��A��A^5A5?A�A�A�aA@OA�jA��Am]A�A��A��AA A�?A^�A��A�)A�\A*0A
�EA
��A
c A
�A	��A	��A��A$�A�A�A�`AH�A�8A�~A�	A�A+�A/A��AC-A��AbNA ��A �eA m]A +�A 4�A �@��3@���@��@��>@��~@���@�$�@���@�;@�tT@�x@�ی@���@�H@�,=@�@��@��@��.@��@���@��5@��6@���@�s�@��@�@�m]@�Y@�~�@�"@��8@��@�x@�V�@��@�}�@�ȴ@��K@��@�W�@�J@�F@�~�@�!-@�@�oi@�Ta@��@�[W@迱@��#@�b@��@�@���@�|@��@�u%@�@�V@�!-@�{�@ߏ�@�%F@��U@�y>@�@�@��@��m@ݜ�@�S�@ܲ�@�"h@�ƨ@�s@�֡@ڎ�@���@�J�@���@�q�@�D�@�zx@�ں@�}V@��@�~�@��@��@ӊ	@Ґ.@�8�@�O@���@έ�@�@�@;w@̓{@�e�@�F�@��@̰!@�A�@�w2@��m@�U2@�  @�^�@���@ȡb@�J@Ǌ	@��K@�C�@ŝ�@�+�@ě�@�l�@�!�@��@��@£�@�V�@�  @���@�y�@�C@��+@���@�qv@�(�@��@��h@�M@��@��'@�$t@��5@��@��u@�1'@�>�@�1�@���@��F@��r@�t�@�.I@��@��.@�tT@�bN@�,=@�	�@���@�q@��A@��@�dZ@�ی@�p;@���@��K@��@��A@�q�@�\�@�<�@�ݘ@�W?@�@��9@��!@�� @�n�@�;�@��.@��Q@�O�@���@�?@��@�.I@��/@��b@�6@��o@�\�@��@��,@�ff@�($@���@�ϫ@���@�j�@�@��u@�z@�i�@�S�@�?�@�@��h@��@��@��E@�ȴ@���@�}V@�6�@��N@���@�<6@���@�Ov@���@��~@�;d@��@�:�@�� @���@���@�s�@��@���@��+@�ff@�I�@�:*@��@���@��@���@�9X@���@�y�@�G�@��2@���@�r�@�PH@��@���@�O@��@���@�Ta@��@���@�H�@�1�@�;@��@���@�~�@�~@���@��3@���@��C@�A @��z@�PH@�/�@���@��@�S�@�o@��@���@�D�@��@��j@��@��V@�s@�c�@��@��@��z@�W�@�@��+@���@���@�p�@�;d@�C@��P@��H@��@���@�S�@�!�@��N@�c@�;@��\@�Ft@���@��"@�m]@�1�@��`@��.@�I�@�"h@��@��z@���@�v`@�E9@�@���@��@�r�@�*�@��@��w@��F@���@���@���@��'@���@��@��2@�� @�7�@��@���@���@�8�@���@�c�@�$@��z@�X@��|@���@�J�@�C-@�@�@��@��}@�\)@�@��2@���@���@�`�@�9X@�4@��@�*@~��@~5?@~e@}��@}�7@}F@}�@|�9@{��@{o�@{\)@{S�@{!-@z�F@z4@y�@y7L@xz�@xS�@x2�@x@w��@v�@v �@uX@t�@tU2@s�@s�$@sP�@sC@r�b@rH�@q��@qe,@p��@pFt@oƨ@o,�@n�c@n�!@nq�@m��@m&�@l��@lM@k��@k;d@j��@j
�@i��@h��@h6@g� @g��@g~�@g>�@f�"@f�<@f��@fl�@f�@e��@d��@d!@cy�@b�X@aԕ@a��@a\�@aV@`��@`�u@`D�@`�@_�[@_33@_Y@_�@^�@^�F@]�@]�X@]�@]�@\�u@\�@[�@@[Mj@Z�2@Z�A@ZB[@Z �@Y�@Y�n@Y#�@X�5@X�@X�@WP�@V��@U��@T��@T�@Tu�@T]d@TD�@T�@SO@R��@RkQ@R0U@R{@Q��@Qk�@QG�@PɆ@P��@P_@P-�@O��@O��@O��@OE9@O�@N҉@N?@M��@M�d@M��@MDg@L�f@L�@Lj@L>B@K�m@K�@Ko�@KP�@K=@J�@J��@Jh
@J0U@I�Z@I��@I��@IIR@Hw�@H/�@G�a@F�@F�F@F0U@E��@E4@E�@EV@E�@Dl"@D7�@C��@C8@Bd�@Au�@AG�@@�@@��@@`�@@	�@?��@?]�@?'�@>�x@>Ov@=�@=j@<��@<�O@<�I@<��@<S�@<�@;�&@;y�@;RT@:�,@:�b@:��@:$�@9�H@9�M@9e,@95�@9@@8��@8_@7�A@7�w@7t�@7@O@7
=@6�L@6�A@6n�@6	@5w2@4��@4��@4K^@4b@3�m@3ƨ@3��@2�@2��@2��@2s�@2V@2;�@2($@2&�@2!�@1�D@1�9@1�z@1�@1w2@1O�@1(�@1�@0�/@0��@0�Y@0e�@02�@0b@/�r@/��@/��@/_p@.��@.��@.W�@-��@-�@-��@-�"@-N<@-+@,�P@,�@,�[@,�e@,4n@+�{@+Mj@*�@*��@*��@*��@*i�@*4@)��@)�@)�^@)��@)S&@(�|@(��@(�@(�@(�@(��@(D�@(M@'�Q@'��@'j�@'S�@'"�@&�@&�@&ں@&��@&�}@&��@&s�@&YK@&@%��@%ϫ@%��@%a�@%<6@$�p@$��@$A�@#ƨ@#v`@#4�@"�@"�x@"&�@!��@!��@!a�@!*0@!�@ ��@ �o@ M@  �@ G@ �@�W@�g@ƨ@��@)_@��@�x@s�@#:@��@�z@}�@<6@��@��@bN@'R@@��@RT@�<@h
@��@ϫ@w2@\�@/@��@�4@�@%�@�@�K@�*@~�@�@�@�@�m@��@�b@�@�t@��@m]@T�@�@��@�p@��@w�@1'@�
@�@{J@n/@>�@�@��@\�@C�@	@�t@��@e,@�@��@�o@2�@	�@��@dZ@4�@�@�M@͟@��@�R@��@E�@�@�@�3@�@Dg@��@Ɇ@��@�@_@9X@x@�@�A@�m@�Q@�K@�$@qv1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aϩ_AϫAϬqAϮ�AϰUAϵAϸ�Aϸ�AϹXAϺ*AϺ�Aϻ�A�� A�ĜA��tA�ŢA���A��?A���A��9A�ʌA��#A���AϽ�A�4nAǗ�AǺ*A�z�A�@OA�XyA�/OA� \A�
�A�)�A���A�ZQA�@A���A�oA��zA�tA���A�0!A�oA��0A�یA��oA��@A��A�ŢA� �A��OA�I�A��A��IA�[�A�~�A�� A�!-A���A���A��RA��?A��FA�aA�!-Aw�As�PAl�Ad�#AbA�A^��AY��AT��AR'�AOqAN��AL&�AIAG��AG>BAF��AB��A?{A<��A;j�A:�A9iDA8�A5��A4y�A2a�A1K^A1'RA0�A0e,A.��A-��A*J�A'g�A%�6A&�A&zA'DgA(�"A*��A,�{A-�[A.=�A.�A/�A/qA/$tA/=A/*�A/eA.�uA.J�A.�A.�yA.ߤA.�5A.�oA.��A.iDA-�0A,�!A+��A+A*��A)}VA(��A'��A'[WA'!-A&��A%�A%b�A$>BA#��A#�A"�A"X�A!��A �A��A��A��A\�A%�A��AXyA�9A/�A�5At�Av�A��A4A-�AݘA�@A� A�dA�A-wAp�A�$A4A�ZA'�A�A��A��A�AxlADgA�A��A1�A�A{JA"�A�UAYA��A��A^5A5?A�A�A�aA@OA�jA��Am]A�A��A��AA A�?A^�A��A�)A�\A*0A
�EA
��A
c A
�A	��A	��A��A$�A�A�A�`AH�A�8A�~A�	A�A+�A/A��AC-A��AbNA ��A �eA m]A +�A 4�A �@��3@���@��@��>@��~@���@�$�@���@�;@�tT@�x@�ی@���@�H@�,=@�@��@��@��.@��@���@��5@��6@���@�s�@��@�@�m]@�Y@�~�@�"@��8@��@�x@�V�@��@�}�@�ȴ@��K@��@�W�@�J@�F@�~�@�!-@�@�oi@�Ta@��@�[W@迱@��#@�b@��@�@���@�|@��@�u%@�@�V@�!-@�{�@ߏ�@�%F@��U@�y>@�@�@��@��m@ݜ�@�S�@ܲ�@�"h@�ƨ@�s@�֡@ڎ�@���@�J�@���@�q�@�D�@�zx@�ں@�}V@��@�~�@��@��@ӊ	@Ґ.@�8�@�O@���@έ�@�@�@;w@̓{@�e�@�F�@��@̰!@�A�@�w2@��m@�U2@�  @�^�@���@ȡb@�J@Ǌ	@��K@�C�@ŝ�@�+�@ě�@�l�@�!�@��@��@£�@�V�@�  @���@�y�@�C@��+@���@�qv@�(�@��@��h@�M@��@��'@�$t@��5@��@��u@�1'@�>�@�1�@���@��F@��r@�t�@�.I@��@��.@�tT@�bN@�,=@�	�@���@�q@��A@��@�dZ@�ی@�p;@���@��K@��@��A@�q�@�\�@�<�@�ݘ@�W?@�@��9@��!@�� @�n�@�;�@��.@��Q@�O�@���@�?@��@�.I@��/@��b@�6@��o@�\�@��@��,@�ff@�($@���@�ϫ@���@�j�@�@��u@�z@�i�@�S�@�?�@�@��h@��@��@��E@�ȴ@���@�}V@�6�@��N@���@�<6@���@�Ov@���@��~@�;d@��@�:�@�� @���@���@�s�@��@���@��+@�ff@�I�@�:*@��@���@��@���@�9X@���@�y�@�G�@��2@���@�r�@�PH@��@���@�O@��@���@�Ta@��@���@�H�@�1�@�;@��@���@�~�@�~@���@��3@���@��C@�A @��z@�PH@�/�@���@��@�S�@�o@��@���@�D�@��@��j@��@��V@�s@�c�@��@��@��z@�W�@�@��+@���@���@�p�@�;d@�C@��P@��H@��@���@�S�@�!�@��N@�c@�;@��\@�Ft@���@��"@�m]@�1�@��`@��.@�I�@�"h@��@��z@���@�v`@�E9@�@���@��@�r�@�*�@��@��w@��F@���@���@���@��'@���@��@��2@�� @�7�@��@���@���@�8�@���@�c�@�$@��z@�X@��|@���@�J�@�C-@�@�@��@��}@�\)@�@��2@���@���@�`�@�9X@�4@��@�*@~��@~5?@~e@}��@}�7@}F@}�@|�9@{��@{o�@{\)@{S�@{!-@z�F@z4@y�@y7L@xz�@xS�@x2�@x@w��@v�@v �@uX@t�@tU2@s�@s�$@sP�@sC@r�b@rH�@q��@qe,@p��@pFt@oƨ@o,�@n�c@n�!@nq�@m��@m&�@l��@lM@k��@k;d@j��@j
�@i��@h��@h6@g� @g��@g~�@g>�@f�"@f�<@f��@fl�@f�@e��@d��@d!@cy�@b�X@aԕ@a��@a\�@aV@`��@`�u@`D�@`�@_�[@_33@_Y@_�@^�@^�F@]�@]�X@]�@]�@\�u@\�@[�@@[Mj@Z�2@Z�A@ZB[@Z �@Y�@Y�n@Y#�@X�5@X�@X�@WP�@V��@U��@T��@T�@Tu�@T]d@TD�@T�@SO@R��@RkQ@R0U@R{@Q��@Qk�@QG�@PɆ@P��@P_@P-�@O��@O��@O��@OE9@O�@N҉@N?@M��@M�d@M��@MDg@L�f@L�@Lj@L>B@K�m@K�@Ko�@KP�@K=@J�@J��@Jh
@J0U@I�Z@I��@I��@IIR@Hw�@H/�@G�a@F�@F�F@F0U@E��@E4@E�@EV@E�@Dl"@D7�@C��@C8@Bd�@Au�@AG�@@�@@��@@`�@@	�@?��@?]�@?'�@>�x@>Ov@=�@=j@<��@<�O@<�I@<��@<S�@<�@;�&@;y�@;RT@:�,@:�b@:��@:$�@9�H@9�M@9e,@95�@9@@8��@8_@7�A@7�w@7t�@7@O@7
=@6�L@6�A@6n�@6	@5w2@4��@4��@4K^@4b@3�m@3ƨ@3��@2�@2��@2��@2s�@2V@2;�@2($@2&�@2!�@1�D@1�9@1�z@1�@1w2@1O�@1(�@1�@0�/@0��@0�Y@0e�@02�@0b@/�r@/��@/��@/_p@.��@.��@.W�@-��@-�@-��@-�"@-N<@-+@,�P@,�@,�[@,�e@,4n@+�{@+Mj@*�@*��@*��@*��@*i�@*4@)��@)�@)�^@)��@)S&@(�|@(��@(�@(�@(�@(��@(D�@(M@'�Q@'��@'j�@'S�@'"�@&�@&�@&ں@&��@&�}@&��@&s�@&YK@&@%��@%ϫ@%��@%a�@%<6@$�p@$��@$A�@#ƨ@#v`@#4�@"�@"�x@"&�@!��@!��@!a�@!*0@!�@ ��@ �o@ M@  �@ G@ �@�W@�g@ƨ@��@)_@��@�x@s�@#:@��@�z@}�@<6@��@��@bN@'R@@��@RT@�<@h
@��@ϫ@w2@\�@/@��@�4@�@%�@�@�K@�*@~�@�@�@�@�m@��@�b@�@�t@��@m]@T�@�@��@�p@��@w�@1'@�
@�@{J@n/@>�@�@��@\�@C�@	@�t@��@e,@�@��@�o@2�@	�@��@dZ@4�@�@�M@͟@��@�R@��@E�@�@�@�3@�@Dg@��@Ɇ@��@�@_@9X@x@�@�A@�m@�Q@�K@�$@qv1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�UB�UB�UB�oB��B�!B�;B�B�!B�!B�;B�;B��B�B�B�B�iB�B�iB�OB�B�B�;B��B	��B
��B
�qBm�B�?B�FB��B��B�+BǔB�2B�'B�WB�B�eB�sB�B�>B�'BB1B@B	B�B�`BޞB�qBܬB�kB�SB�:B�MB�B��B�:Ba�B!�B
ÖB
��B
��B
KDB	��B	�UB	�=B	fLB	EmB	;0B	.�B	/B	�B	B�B�B�BؓB��B�bB̈́BĜB�qB��B��B�BÖB��B�@B��B��B�B��BܬB��B�>B�B��B�?B	-B	"�B	2�B	F�B	j�B	��B	ݲB
 �B
�B
OB
'RB
*KB
1[B
;dB
F�B
LB
MjB
L�B
YB
fLB
p�B
y	B
}B
��B
��B
��B
z*B
vB
sMB
qvB
lWB
h>B
dB
a-B
a�B
e�B
_VB
d�B
bB
f�B
`�B
^jB
^�B
[#B
S[B
M�B
F�B
C�B
C�B
DB
C-B
EmB
EB
@iB
>�B
?�B
;JB
9$B
7fB
3�B
-B
5tB
NB
V�B
aB
\]B
X_B
T�B
R B
S�B
`'B
`�B
a-B
cB
b�B
a�B
b4B
b�B
bB
_�B
]�B
]�B
\B
Y1B
T�B
S�B
VB
X�B
W�B
W�B
WsB
ZB
Z�B
X�B
W�B
V�B
U�B
T�B
T,B
R�B
PbB
M�B
J�B
KxB
N�B
O�B
N�B
O(B
Q B
Q4B
P�B
OvB
K�B
F�B
I7B
NB
NVB
M�B
M6B
K)B
J	B
G�B
?.B
8�B
5�B
4�B
3MB
2�B
2-B
1B
0�B
/�B
2aB
2|B
1'B
/OB
-�B
/ B
-�B
-B
+�B
+B
)�B
(�B
'�B
'�B
'8B
&�B
&fB
&2B
%�B
%�B
%`B
$�B
$B
#�B
#�B
!�B
!�B
!|B
!�B
 �B
 \B
�B
B
OB
B
�B
~B
/B
xB
B
�B
#B
�B
QB
7B
�B
�B
B
1B
�B
B
$B
B
�B
,B
�B
�B
�B
B
�B
 B
�B
B
BB
�B
�B
�B
BB
�B
}B
HB
bB
�B
B
4B
4B
�B
�B
�B
(B
\B
�B
�B
.B
�B
bB
B
B
�B
4B
�B
�B
�B
hB
�B
vB
�B
�B
�B
xB
)B

�B

�B

XB
	�B
	�B

	B

�B
B
^B
�B
�B
�B
�B
^B
B
B
�B
6B
�B
0B
�B

�B

	B

#B

#B

�B

=B
	�B
	�B
B

�B
�B
dB
B
�B
�B
�B
pB
B
�B
�B
�B
PB
�B
�B
�B
xB
�B
B
JB
B
PB
�B
�B
0B
�B
�B

rB
	�B
	lB
	�B
	�B
	�B

XB

XB
	lB
	B
�B
�B
�B

=B
B
�B
~B
B
B
B
�B
�B
�B
�B
vB
HB
\B
\B
B
B
�B
�B
(B
(B
vB
�B
�B
�B
�B
�B
�B
B
bB
bB
bB
HB
.B
bB
4B
�B
�B
�B
�B
�B
�B
SB
KB
�B
eB
eB
�B
�B
�B
B
�B
kB
QB
�B
�B
�B
B
CB
�B
�B
�B
�B
�B
~B
�B
�B
�B
�B
xB
xB
�B
xB
�B
jB
�B
�B
�B
 �B
!HB
!B
 �B
!HB
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!HB
 �B
 �B
 �B
 �B
!B
!|B
!�B
!�B
"hB
#nB
$�B
%`B
%`B
%�B
%�B
%�B
&�B
'�B
(�B
(
B
($B
)_B
(XB
(
B
(�B
)DB
)�B
)�B
)�B
)yB
)�B
)�B
*eB
*KB
)�B
)�B
*�B
+�B
,�B
,�B
-�B
./B
./B
.}B
/B
/�B
/�B
0B
0UB
1'B
1[B
1�B
2-B
2�B
3hB
3�B
3�B
4TB
4�B
4�B
4�B
4�B
4nB
4nB
4TB
4B
5tB
5tB
6B
6�B
6�B
6�B
72B
7fB
88B
8lB
8�B
8�B
9>B
9�B
:^B
:xB
:^B
:B
:�B
:�B
;B
:�B
;B
;JB
;B
;�B
;�B
;�B
;�B
;�B
=B
=VB
=VB
=qB
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
?B
?HB
?cB
?�B
@iB
@OB
@OB
@OB
@�B
AoB
A�B
A�B
B'B
B�B
B�B
CB
CGB
C{B
C�B
DB
D�B
D�B
EB
EB
E9B
E9B
ESB
ESB
EmB
FB
FYB
E�B
FYB
GEB
GEB
G_B
G�B
G�B
HfB
IB
I7B
IRB
IlB
I�B
I�B
I�B
I�B
I�B
J#B
J=B
J�B
KDB
KxB
K�B
L0B
LJB
LJB
L�B
L�B
L�B
MB
MB
MjB
M�B
M�B
M�B
M�B
NB
N�B
N�B
N�B
OB
O\B
O�B
O�B
P.B
P�B
P�B
P�B
Q B
QB
QhB
Q�B
Q�B
Q�B
Q�B
RTB
R�B
S�B
T,B
T,B
TFB
TaB
TaB
TaB
UB
U�B
U�B
V9B
V9B
V�B
V�B
V�B
WsB
W�B
W�B
W�B
XEB
XyB
X�B
X�B
X�B
Y1B
YKB
YB
YB
YB
Y�B
ZB
Z7B
Z7B
Z7B
ZkB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[#B
[qB
[qB
[�B
[�B
\)B
[�B
[=B
\�B
\CB
[�B
[�B
\CB
\�B
\xB
\�B
\�B
]�B
]IB
]~B
]~B
^OB
^OB
^OB
^�B
^�B
^�B
_VB
_�B
_�B
_�B
`\B
`�B
aHB
a|B
b4B
bNB
bhB
bhB
b�B
cB
cnB
c�B
dB
d�B
d�B
eB
e�B
f2B
f�B
f�B
gB
gB
g8B
g�B
g�B
g�B
g�B
h>B
h�B
iyB
i�B
i�B
j�B
kQB
k�B
k�B
lB
l"B
l"B
l"B
l"B
l�B
m)B
m]B
m]B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
nIB
n/B
nIB
n}B
n}B
n�B
n�B
o B
n�B
o5B
o5B
oOB
o�B
o�B
p!B
pUB
pUB
poB
pUB
p�B
p�B
p�B
p�B
poB
pUB
pUB
poB
p�B
p�B
poB
poB
pUB
pB
p;B
p;B
p;B
pUB
pUB
p�B
p�B
q'B
q'B
qAB
q'B
q'B
qvB
q�B
q�B
q�B
r-B
raB
r�B
s�B
tTB
t9B
t�B
t�B
t�B
uB
t�B
uZB
u�B
u�B
vB
v+B
vFB
v�B
v�B
wB
wLB
w2B
wB
w2B
wLB
wfB
wfB
w�B
wfB
wLB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
xB
x8B
x�B
x�B
y>B
yXB
y�B
y�B
y�B
y�B
y�B
y�B
zB
z^B
z�B
z�B
z�B
{dB
{�B
|�B
}B
}"B
}�B
}�B
}�B
~(B
~BB
~wB
B
cB
}B
�B
�B
�OB
��B
��B
��B
��B
��B
��B
��B
�B
�'B
�AB
��B
��B
�B
��B
�aB
��B
��B
��B
��B
��B
��B
�B
�MB
�gB
�gB
��B
�B
�SB
�SB
��B
�B
�?B
�tB
��B
��B
�B
�+B
�_B
�B
�KB
�1B
�KB
�KB
��B
��B
��B
��B
��B
�7B
��B
��B
�lB
��B
��B
��B
��B
��B
��B
��B
�#B
�#B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�*B�DB��B��B��B��B��B�B��B��B�XB�sB�XB�>B�XB�$B�$B��B�B�"B��B	��B
�B
��Bn�B��B�hB�B��B��B�MBҽB�B�fB�FB�B�B��B�NB��B3B�BBEB��B�vBؓB�B�B�B�bB͟B�cB�TB�B��BcTB$@B
�GB
�=B
�KB
O\B	��B	��B	�B	f�B	CB	8�B	-�B	WB	
#B	�B�B�B�'B�B��B�^B�DB��B�$B�B�B��B�wB�[B��B�<B��B�YB�9B֡B�B�B�B�B�B��B	�B	*KB	=�B	aHB	�eB	��B	��B
�B
�B
�B
"�B
*B
4TB
?�B
E9B
F?B
EB
Q�B
_B
iDB
q�B
u�B
|jB
~wB
z�B
tB
o�B
l�B
k�B
fB
bB
]dB
Z7B
[WB
_�B
YB
^jB
[�B
`BB
Y�B
W�B
XEB
UB
MB
G�B
?�B
<�B
<�B
=<B
<�B
>�B
>�B
9�B
8B
9�B
5%B
2�B
1[B
-�B
%�B
-CB
F�B
O�B
Z�B
VB
R B
M�B
J�B
L�B
X�B
Y�B
ZB
[�B
[�B
[	B
[#B
[�B
[�B
X�B
W
B
V�B
UgB
R�B
M�B
L�B
OB
Q�B
P�B
P}B
PbB
SuB
S�B
Q�B
P�B
O�B
N�B
M�B
M�B
LB
I�B
GB
C{B
D�B
HB
H�B
G�B
HB
J#B
J#B
I�B
IB
EmB
?�B
BB
G+B
G�B
F�B
F�B
D�B
C�B
B[B
9	B
2-B
/ B
./B
,qB
+�B
+QB
*B
)�B
(�B
+QB
+�B
*B
(sB
&�B
(
B
'B
&2B
$�B
$&B
# B
"B
 �B
 �B
 B
pB
!B
�B
�B
�B
OB
�B
dB
/B
�B
	B
�B
�B
�B
B
�B
KB
EB
?B
�B
�B
�B
SB
�B
�B
B
FB
�B
[B
@B
�B
�B
oB
:B
�B
hB
bB
�B
vB
�B
�B
�B
�B
DB

�B

	B
	�B
	RB
�B
fB
�B
�B
KB
	�B
	lB
	RB
	�B
	�B

XB

rB

XB
	�B
	B
�B
fB
�B
	B
�B
	7B
	7B
	�B
	7B
	RB

�B

rB

�B
)B
)B

�B

rB
	7B
�B
�B
�B
gB
B
�B
�B
{B
B
B
aB
�B
3B
�B
�B
�B
�B
�B
�B
mB
YB
�B
YB
�B
SB
SB
�B
�B
-B
-B
{B
GB
�B
�B
�B
�B
�B
SB
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
B
SB
�B
�B
B
9B
B
%B
�B
�B
9B
�B
�B
�B
�B
�B
�B
�B
�B
�B
aB
AB
�B
�B
�B
�B
aB
B
�B
mB
�B
B
%B
�B
tB
B
+B
�B
	�B
�B
KB
B
1B
�B
�B
B
1B
�B
�B
�B
�B
�B
�B
�B
	7B
	RB
	7B
	RB
	7B
	7B
	�B

XB

�B
�B
�B
�B
B
�B
\B
TB
�B
oB
�B
B
�B
�B
B
�B
uB
&B
�B
�B
B
�B
2B
�B
�B
�B
�B
B
�B
B
B
�B
�B
gB
�B
�B
�B
�B
�B
�B
�B
�B
B
QB
B
�B
QB
�B
kB
�B
�B
�B
�B
�B
�B
kB
7B
�B
�B
�B
�B
B
�B
�B
�B
qB
]B
B
OB
jB
�B
�B
�B
�B
 �B
!�B
 �B
!-B
"NB
!-B
 �B
!�B
"4B
"�B
"�B
"�B
"hB
"�B
"�B
#nB
#TB
#B
#B
$B
$�B
%�B
&B
'B
'B
'8B
'mB
(
B
(�B
(�B
(�B
)*B
*B
*eB
+B
+6B
+�B
,WB
,�B
,�B
-]B
-wB
-]B
-]B
-wB
-CB
-]B
-CB
-B
.}B
.}B
/ B
/�B
/�B
/�B
0UB
0oB
1AB
1vB
1�B
2B
2GB
3B
3hB
3hB
3MB
3B
3�B
3�B
4B
3�B
4B
4B
4TB
4�B
4�B
4�B
4�B
5B
5�B
6FB
6FB
6`B
6zB
6�B
6�B
7B
7�B
7�B
7�B
7�B
7�B
8B
8RB
8RB
8�B
9XB
9>B
9>B
9XB
9�B
:^B
:�B
:�B
;0B
;�B
;�B
;�B
<B
<jB
<�B
="B
=qB
=�B
>B
>B
>BB
>(B
>BB
>]B
>wB
?B
?HB
>�B
?cB
@4B
@4B
@iB
@�B
@�B
A�B
BB
BB
BAB
B[B
B[B
B�B
B�B
B�B
B�B
C-B
CaB
C�B
DMB
D�B
D�B
EB
E9B
E9B
E�B
E�B
E�B
FB
F%B
FtB
F�B
F�B
F�B
F�B
GB
G�B
G�B
G�B
HB
HfB
H�B
H�B
IB
IlB
I�B
I�B
I�B
I�B
JXB
JrB
J�B
J�B
KB
KxB
K�B
L�B
MB
MB
M6B
MPB
MjB
M�B
NB
N�B
N�B
OB
O(B
O�B
O�B
O�B
PHB
P�B
P�B
P�B
QB
QhB
Q�B
Q�B
Q�B
R:B
R:B
RTB
RTB
RoB
R�B
R�B
S&B
S&B
SB
S[B
S[B
S�B
S�B
S�B
S�B
TB
TB
TaB
TaB
T�B
T�B
UMB
T�B
TFB
U�B
UMB
T�B
T�B
U2B
U�B
UMB
UgB
U�B
VmB
VSB
V�B
V�B
WsB
W?B
WYB
W�B
W�B
W�B
XEB
XyB
X�B
X�B
Y1B
Y�B
ZQB
ZkB
[#B
[#B
[WB
[WB
[�B
[�B
\CB
\�B
]B
]~B
]�B
^B
^�B
_!B
_�B
_�B
_�B
_�B
`BB
`vB
`�B
`�B
`�B
a-B
a�B
bhB
b�B
b�B
c�B
d@B
dtB
d�B
d�B
eB
d�B
eB
e,B
e�B
fB
f2B
fLB
fLB
ffB
f�B
ffB
f�B
f�B
f�B
f�B
f�B
f�B
gB
g8B
gB
g8B
g8B
gmB
g�B
g�B
g�B
g�B
h
B
h$B
hXB
h�B
h�B
h�B
i*B
i*B
i_B
i*B
i�B
i�B
i_B
i_B
i_B
i_B
i_B
iDB
i_B
iyB
i_B
iDB
i*B
h�B
i*B
iB
i*B
iDB
i*B
i�B
i�B
i�B
i�B
jB
jB
i�B
jKB
jB
jB
j�B
kB
kQB
k�B
lqB
mB
m)B
m�B
m�B
m�B
m�B
m�B
nIB
n}B
n�B
o B
oB
o5B
o�B
o�B
p!B
p!B
p!B
pB
p!B
pUB
p;B
pUB
poB
pUB
p;B
p;B
poB
poB
poB
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q�B
q�B
rB
r-B
raB
r|B
r|B
r�B
r�B
r�B
r�B
s3B
s�B
s�B
tB
tTB
t�B
utB
u�B
vB
vzB
v�B
vzB
v�B
wB
wLB
xB
xRB
xRB
x�B
x�B
y$B
yXB
yXB
y�B
y�B
y�B
zxB
z�B
z�B
{B
{B
{�B
{�B
|B
{�B
|PB
|�B
|�B
|�B
|�B
|�B
|�B
}B
}<B
}VB
}VB
}�B
~B
~BB
~BB
~�B
~�B
B
cB
}B
�B
�B
�B
�OB
��B
�B
��B
�;B
� B
�oB
��B
��B
��B
��B
�'B
�AB
�uB
�AB
�uB
��B
��B
��B
��B
��B
��B
��B
�B
�aB
�a3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<3|�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<:�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ=PSAL(corrected by recalS & CTM)+deltaS, where deltaS is calculated by OW; PSAL_ADJ_ERR=max(RecalS & CTM & OW error , 0.01(PSS-78))                                                                                                                     SP=0.14(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            r=0.9998(+-0.0000), deepest deltaS=-0.007(+-0.002)(PSS-78); Mapping scale = 8/4,4/2; 0-1500(dbar) is excluded in mapping;                                                                                                                                       Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            OW(Ver.1.1) salinity adjustment is adopted                                                                                                                                                                                                                      202012270047452020122700474520201227004745202012270200402020122702004020201227020040202207271540502022072715405020220727154050  JA  ARFMdecpA30a                                                                20201215153934  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20201215153942  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20201215153944  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20201215153945  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20201215153945  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20201215153945  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20201215153945                      G�O�G�O�G�O�                JA  ARUP                                                                        20201215155209                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20201217000000  CF  PSAL_ADJUSTED_QC@Q�@�  G�O�                JM  ARCAJMQC2.0                                                                 20201226154745  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20201226154745  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20201226170040  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727064050  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091506                      G�O�G�O�G�O�                