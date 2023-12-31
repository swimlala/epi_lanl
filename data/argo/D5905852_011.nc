CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-05-23T09:37:37Z creation;2019-05-23T09:37:39Z conversion to V3.1;2022-08-02T05:12:37Z update;     
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
_FillValue                 �  ]\   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aL   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �8   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ߈   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �l   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �|   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190523093737  20220818081508  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_011                    2C  D   APEX                            8420                            2.11.2                          846 @ؿ�З� 1   @ؿ�I2q @+6_ح���d��c�A 1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�33B���B�33B�33B�  B�33B�  B�  B���B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch33Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D���D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��3D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@���@��AG�A!AA�A`��A���A��HA���A��RA���A�z�A�Q�A�z�B 33B33B33BG�B \)B(\)B0Q�B8\)B@ffBH\)BPffBX=qB`�Bh33Bpz�Bx�HB�#�B�#�B�(�B�#�B�\B��B�(�B�(�B�.B�B�B��=B�Q�B���B�B�B�\)B�{B�G�B�8RB�(�B�  B��B�W
B�
=B��fB�{B�(�B��B��B�#�B�#�B�.B�(�C C
=C\C\C�C
!HC
C�C�C
=C
=C�C�C
C
C
C �C"
C$�C&�C(\C*�C,\C.\C0
=C2\C4(�C6!HC8�C:�C<�C>
=C@
=CB�CD
=CF�CH�CJ�CL�CN{CP�CR{CT\CV
=CX\CZ{C\�C^)C`�Cb�Cd
=Cf�Ch@ Cj�Cl
=Cn
=Cp�Cr�Ct�Cv
=Cx\Cz�C|{C~\C��C��C��C�
=C��C�fC��C�C�C��C��C�fC�C�fC��C��C��C�
=C�fC�fC��C��C��C�C��C��C��C��C��C�fC�C��C��C��C�
=C�fC��C��C�
=C�\C�C��C�C�fC��C�
=C��C�fC�
=C��C�C�fC��C��C�
=C��C��C��C�C�C�
=C�C��C��C��C��C�
=C�fC�C�fC�fC��C��C��C��C�fC�C��C��C�C��C�C��C��C��C�
=C�C��C��C��C�
=C��C�C�fC�fC��C�
=C��C��C�fC��C��C��C��C��C��C��C��C��C��C�
=C��C�
=C��C�\C��C�
=C��C�fC��C��C�
=C�
=C��C��C��C�
=C��D D �3D3D�{DD�{D�D��D�D�3DD�D3D�D
D��D3D��D	�D	�3D
�D
�3D{D��D{D��D�D�3D�D��D�D�D{D�3DD�D�D�3DD�{D�D�DD�D{D�3D{D�{D{D�fDfD��D�D�D
D�D3D��D�D�3DD�fD
D�fD �D ��D!D!�fD"
D"�D#D#�{D$3D$�3D%3D%��D&�D&�D'{D'�HD(�D(��D)3D)��D*D*�
D+D+�{D,D,�{D-3D-�{D.�D.��D/�D/��D0fD0�D1{D1��D2D2�{D3D3�D4{D4��D5�D5��D6�D6�D7�D7��D83D8�D9fD9�D:�D:�3D;3D;�HD<�D<�{D=D=�3D>�D>�3D?{D?��D@�D@�fDA�DA��DB3DB�{DCfDC�fDD�DD��DE{DE�DF�DF�3DG3DG��DH�DH��DIDI�fDJDJ�{DK3DK��DLDL�3DM{DM�DN�DN�fDO�DO��DPfDP��DQDQ�{DR�DR��DSDS�3DT�DT��DU{DU�3DV�DV�{DWfDW��DX�DX��DY�DY��DZDZ�D[D[�D\{D\��D]�D]�D^{D^��D_�D_��D`�D`��DaDa�3Db�Db��Dc�Dc��Dd{Dd��De�De�fDffDf�Dg�Dg�{Dh{Dh��Di�Di��Dj�Dj�Dk3Dk�{Dl�Dl��DmDm�{Dn{Dn�
Do�Do�3Dp{Dp�fDq3Dq��Dr3Dr�{Ds�Ds�Dt�Dt��Du�Du�fDv�Dv��Dw�Dw�RDx{Dx��Dy�Dy�3DzDz�
D{fD{�D|D|�D}{D}�{D~�D~�D3D��D�HD�AHD��HD��HD��D�B=D���D���D�3D�C3D��3D���D� RD�@�D���D�D��D�B�D���D���D�3D�C3D���D��=D�=D�A�D��=D���D��D�B=D���D���D��D�AHD��HD���D�=D�A�D���D���D�=D�B�D���D��HD��D�B=D���D�D��D�A�D���D��=D��D�C3D��=D�D�3D�C3D���D���D�3D�B=D���D���D��D�A�D���D���D��D�B�D���D��HD��D�C�D��=D���D��D�A�D��=D���D�=D�B�D��=D���D�3D�C3D���D���D��D�A�D���D���D��D�B=D���D���D�=D�A�D��HD���D��D�B�D��=D���D��D�A�D��HD��=D��D�B�D���D�D��D�A�D��HD���D�=D�B=D���D���D��D�A�D���D�D��D�A�D���D�D��D�B�D���D��3D�3D�B�D��3D�D��D�B=D���D���D��D�A�D���D���D��D�B=D���D���D��D�B�D��3D��=D��D�A�D���D���D��D�A�D��=D�D�=D�B�D���D��3D��D�B�D���D���D��D�A�D���D���D��D�B�D���D��3D�3D�A�D���D���D��D�B�D��)D��)D�=D�B=D���D��=D��D�A�D��=D���D�=D�B�D��=D��=D��D�A�D��=D��=D��D�A�D��=D�D�3D�B=D��HD�D��D�B�D��3D�D�=D�B�D���D���D��D�B�D��=D���D��D�B�D��=D��=D��D�B�D��=D���D�3D�C�D���D��=D�=D�B=D���D��3D�=D�A�D���D��HD��D�A�D���D���D�=D�B�D��=D���D�HD�AHD���D��3D�=D�AHD��=D��3D�3D�B�D��=D�D��D�B=D���D���D��D�C3D�D���D�HD�A�DÂ�D�D��D�A�Dā�D��HD�HD�A�DŁ�D���D��D�B=Dƃ3D��=D��D�B=Dǁ�D���D��D�AHDȁ�D���D��D�A�DɁ�D���D�=D�C3Dʃ3D���D�3D�B�Dˁ�D��HD�HD�B=D̂�D�D��D�B=D͂�D���D�HD�B=D΂=D�D��D�B�Dς�D���D��D�A�DЁ�D��=D��D�B=Dс�D�D�=D�B=D҂=D��HD� RD�AHDӂ�D��=D�=D�B�DԂ=D��=D��D�B�DՂ�D��HD��D�B=Dց�D���D��D�C3D׃3D���D��D�B�D؁�D��=D��D�A�Dق=D���D��D�AHDڀ�D���D��D�B�Dۂ�D���D��D�@�D܂=D��=D� �D�@�D݂=D���D�3D�C3Dނ�D�D��D�B�D߂�D��=D�=D�A�D���D��)D��D�@RDၚD�D��D�B�D��D���D� �D�B�DわD���D��D�C3D䃅D���D��D�B=D��D�D��D�B=D��D�D��D�A�D�HD��=D��D�B=D肏D��=D�=D�B�D郅D�ÅD��D�AHD�=D��=D�HD�A�D끚D��HD��D�A�D�=D��HD��D�B�D��D��HD�HD�A�D��D�ÅD�3D�B=D�HD���D��D�A�D���D���D��D�A�D�HD��HD��D�B�D�D��=D��D�B=D��D��=D� �D�A�D��D�D��D�C3D���D��=D��D�A�D���D���D��D�@�D���D���D��D�A�D���D�D��D�AHD���D��RD� �D�@�D��HD��=D��D�<)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AۑhA�Q�A�MA��Aژ�A�dZA�YA��)A��Aٵ�A٨XA٦�A٦�A١bAٝ�AٚkAٛ=Aٝ�AٝIA٠�A٥FA٪eAٳ3A���A��WA��A��A��A���AٿAلAـ4Aٖ�A��A��BA��VA�bA�MA�$�A�;�A�>A��9A��dA�D�A���A��EA�ɆAĝ~A�-�A�uA�v�A��xA���A��A��A���A��A�{�A��!A��A�RTA���A���A�	A���A���A��*A��eA��"A��}A���A���A�(�A�A�MjA��A��A���A�c A�oA�K^A���A���A���A�҉A�,�A�r�A�^A��A��BA�=A33A|]�AyU2Awr�At�AnE�AkZAh�OAf�PAdL�A`�
A]ƨAZ�AV�"AV&�AT�[AQ�AM��AJ+AH�AH[WAF�AC��AB��AA�AA>%A;_�A:C-A8}�A7A5��A4[�A27A0��A0p�A0�A/��A.��A-��A,�)A+��A+�A+~�A+Y�A*�$A)*�A&�WA&v`A&A$�@A%�A$��A$�A$ںA$�QA%�A%8A%h
A%C�A%'RA%�A$�2A$T�A#�QA#�7A"��A!�jA!!-A Q�A 	A�6A��A8A�mAX�A�PA��Ae�A7LA�A��A;dA�A  A{A6zAr�A��A4�AsA_pAR�Am�A<�A�"A#�A1�A:�A��A6zA��A1'AMjA�A�A��Ah�A�A�}A  A
qA
T�A
#�A

=A
�$A2aA>BAA
�!A
�@A
��A
�oA
VmA	��A	xlA	1�A�A��A9XA+A	lA�jA��A\�A��A�AoA�fARTA.IA%�A�nAA VA [W@��*@��@���@�&@���@��@�&�@�!-@��X@��@���@�|�@���@�Ta@�1@�|�@�v�@��B@�#:@�n@��[@��Z@�l�@���@�H�@�V�@��Q@��D@�%�@�@�|�@�Y�@��@�ϫ@�e�@�"�@�|�@�6@��@��@��Q@�f�@ޠ�@��@�4@���@ݧ�@�s@�u�@��@�@ۜ@ۖS@�|�@��@�9X@��r@٠�@��@�ȴ@ػ�@ؗ�@؀�@��@���@�Vm@��'@�D�@��@�X�@���@ң@�$@�y�@р4@�-w@��f@�+�@��,@У@�;�@�ی@��@�qv@�,�@��@�K^@��T@�O�@̯O@�bN@��@��@��d@˽�@ˈf@�Ĝ@�#:@��a@�C�@��[@�!@Ǘ�@�O@Ƒ�@�'R@���@ű[@�Y�@Ĺ�@�ϫ@Ò:@�j�@�J�@��s@�;�@��@���@�I�@���@�T�@��@��@��R@��1@�c�@��@��S@�_p@�<6@�ߤ@�*�@��@�hs@�"�@��8@��u@�Ft@� �@�zx@�5�@��@��@��@�q@���@��X@�g8@�@���@�2a@��@��@��r@���@��0@���@�~�@�C�@�;d@�"�@���@�_@��@��r@���@�y�@�Q�@�6z@�@@�\�@���@���@��@�S&@�*0@���@��X@�h
@�@�_@���@��V@�(�@��	@�ی@��z@�u�@�Z@��@���@��@�Q�@�@��`@���@��@��6@�(@�h�@��@�A @���@�E�@��@��@��^@�c@�,�@��)@�v�@��@�A @��)@�.�@���@�Dg@�
=@���@��r@�PH@�1'@�
�@���@�u�@�B�@��@�@���@�M@���@���@�`B@��8@�r�@�� @�u�@���@�L0@��@���@���@��*@���@��:@�u�@�RT@�	l@��Y@�Q@�($@��
@���@��4@��@���@��@�r�@��@���@��#@��K@���@��=@���@���@�zx@�j@�hs@�RT@���@���@��Y@�d�@�<�@��o@�˒@��@@�j�@���@���@��+@���@�qv@�+@� \@��v@���@��@�hs@��,@���@���@���@�g8@��@���@�|@�_p@�H�@�4�@�1�@�"�@��X@���@�_@�3�@��@��F@�33@��}@�c @�9X@��@�}�@��@��L@�d�@��@���@���@�F@��@��@�?�@�'R@�	@���@��=@�_p@��@�u%@��@���@�_p@�Ɇ@�u@��6@��d@���@�s@�W?@��P@���@�Xy@�#:@�e@�@~�8@~($@}�S@} \@|ی@|�e@|V�@|%�@{��@{�	@z��@zGE@y��@y@x�j@xg8@xA�@w�@wy�@w6z@v�c@v�\@v.�@u}�@t>B@sn/@r�@r͟@r�!@r�r@rd�@q�)@qzx@qX@p�4@o@O@n��@n_@m�'@mQ�@m!�@l�E@lH@kݘ@ke�@j��@j�@j{�@j3�@i�T@i��@i:�@h��@h[�@g�Q@gy�@f�m@fa|@f+k@e�H@es�@d�5@c�A@cv`@cE9@b�c@b-@b �@a��@a�@a@`c�@_"�@^Z�@]�@]�~@]*0@\��@\G@[��@[e�@[P�@[J#@[J#@[�@Z\�@X�f@W��@V��@V� @V~�@VQ@U�H@U!�@TM@S�q@SE9@R�\@Q�z@QN<@P�e@O� @O)_@N�'@N��@N$�@N �@M�H@MVm@L�?@K�@KiD@K�@J��@Jq�@I�o@I��@IS&@I@@Hj@G��@G�V@G@O@F�6@E��@EDg@E@D�U@D�o@D!@C��@C��@C�{@CO@C)_@C�@C�@B�8@B��@B�@B��@Bv�@B1�@A�9@A�n@AB�@@��@@�@@e�@@9X@?�;@?|�@?�@>�@>��@>��@>_�@>5?@=��@=c@=0�@<ѷ@<��@<y>@<PH@<M@;�0@;F�@:�8@:��@:��@:v�@:_�@::*@:#:@:�@9�>@9��@9x�@9c�@94@8��@8�o@87�@7�@7�q@7�*@7K�@6�"@6��@6�<@6��@6�+@6)�@5�@5s�@5L�@4��@4�?@4�@4'R@3��@3�$@3s@3W?@3K�@3+@3�@2�2@2�R@2xl@2R�@2�@1��@1�'@1��@1w2@1S&@1*0@0��@0�@0 �@/�]@/�K@/�@.�h@.�L@.�r@.V@.H�@.B[@.;�@-�T@-�@-p�@-\�@-+�@,��@,�$@,�.@,[�@,�@+�@+�:@+�{@+Z�@++@+S@*�@*�@*�!@*�x@*�1@*�\@*Ov@*	@)�Z@)��@)��@)L�@(�9@(N�@($@(1@'��@'�@'>�@'!-@'�@&�H@&��@&kQ@&u@%�3@%��@%��@%��@%��@%%F@$�@$��@$]d@$,=@$1@#��@#�[@#�k@#t�@#�@"�6@"B[@"�@!�.@!��@!��@!�h@!x�@!hs@!N<@!q@ �@ ��@ �u@ U2@ �@� @�V@O@�@�@��@~�@W�@Q@Ov@0U@rG@Dg@?}@:�@%F@�@�@�Y@Xy@I�@>B@-�@'R@"h@�r@��@o�@;d@�@͟@�\@q�@Ta@+k@J@��@��@�@�"@�@�|@֡@�4@��@[�@6@�@@�@��@�[@�[@�	@]�@6z@!-@�@�H@��@�@H�@@��@S&@?}@&�@�E@�I@tT@1'@�g@�@��@dZ@�@��@��@kQ@M�@4@��@��@��@��@|@a�@O�@A @-w@�@�	@��@��@z�@M@�@�]@�@�&@��@��@�@��@�P@��@J#@�,@�h@�L@��@v�@p;@\�@C�@+k111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AۑhA�Q�A�MA��Aژ�A�dZA�YA��)A��Aٵ�A٨XA٦�A٦�A١bAٝ�AٚkAٛ=Aٝ�AٝIA٠�A٥FA٪eAٳ3A���A��WA��A��A��A���AٿAلAـ4Aٖ�A��A��BA��VA�bA�MA�$�A�;�A�>A��9A��dA�D�A���A��EA�ɆAĝ~A�-�A�uA�v�A��xA���A��A��A���A��A�{�A��!A��A�RTA���A���A�	A���A���A��*A��eA��"A��}A���A���A�(�A�A�MjA��A��A���A�c A�oA�K^A���A���A���A�҉A�,�A�r�A�^A��A��BA�=A33A|]�AyU2Awr�At�AnE�AkZAh�OAf�PAdL�A`�
A]ƨAZ�AV�"AV&�AT�[AQ�AM��AJ+AH�AH[WAF�AC��AB��AA�AA>%A;_�A:C-A8}�A7A5��A4[�A27A0��A0p�A0�A/��A.��A-��A,�)A+��A+�A+~�A+Y�A*�$A)*�A&�WA&v`A&A$�@A%�A$��A$�A$ںA$�QA%�A%8A%h
A%C�A%'RA%�A$�2A$T�A#�QA#�7A"��A!�jA!!-A Q�A 	A�6A��A8A�mAX�A�PA��Ae�A7LA�A��A;dA�A  A{A6zAr�A��A4�AsA_pAR�Am�A<�A�"A#�A1�A:�A��A6zA��A1'AMjA�A�A��Ah�A�A�}A  A
qA
T�A
#�A

=A
�$A2aA>BAA
�!A
�@A
��A
�oA
VmA	��A	xlA	1�A�A��A9XA+A	lA�jA��A\�A��A�AoA�fARTA.IA%�A�nAA VA [W@��*@��@���@�&@���@��@�&�@�!-@��X@��@���@�|�@���@�Ta@�1@�|�@�v�@��B@�#:@�n@��[@��Z@�l�@���@�H�@�V�@��Q@��D@�%�@�@�|�@�Y�@��@�ϫ@�e�@�"�@�|�@�6@��@��@��Q@�f�@ޠ�@��@�4@���@ݧ�@�s@�u�@��@�@ۜ@ۖS@�|�@��@�9X@��r@٠�@��@�ȴ@ػ�@ؗ�@؀�@��@���@�Vm@��'@�D�@��@�X�@���@ң@�$@�y�@р4@�-w@��f@�+�@��,@У@�;�@�ی@��@�qv@�,�@��@�K^@��T@�O�@̯O@�bN@��@��@��d@˽�@ˈf@�Ĝ@�#:@��a@�C�@��[@�!@Ǘ�@�O@Ƒ�@�'R@���@ű[@�Y�@Ĺ�@�ϫ@Ò:@�j�@�J�@��s@�;�@��@���@�I�@���@�T�@��@��@��R@��1@�c�@��@��S@�_p@�<6@�ߤ@�*�@��@�hs@�"�@��8@��u@�Ft@� �@�zx@�5�@��@��@��@�q@���@��X@�g8@�@���@�2a@��@��@��r@���@��0@���@�~�@�C�@�;d@�"�@���@�_@��@��r@���@�y�@�Q�@�6z@�@@�\�@���@���@��@�S&@�*0@���@��X@�h
@�@�_@���@��V@�(�@��	@�ی@��z@�u�@�Z@��@���@��@�Q�@�@��`@���@��@��6@�(@�h�@��@�A @���@�E�@��@��@��^@�c@�,�@��)@�v�@��@�A @��)@�.�@���@�Dg@�
=@���@��r@�PH@�1'@�
�@���@�u�@�B�@��@�@���@�M@���@���@�`B@��8@�r�@�� @�u�@���@�L0@��@���@���@��*@���@��:@�u�@�RT@�	l@��Y@�Q@�($@��
@���@��4@��@���@��@�r�@��@���@��#@��K@���@��=@���@���@�zx@�j@�hs@�RT@���@���@��Y@�d�@�<�@��o@�˒@��@@�j�@���@���@��+@���@�qv@�+@� \@��v@���@��@�hs@��,@���@���@���@�g8@��@���@�|@�_p@�H�@�4�@�1�@�"�@��X@���@�_@�3�@��@��F@�33@��}@�c @�9X@��@�}�@��@��L@�d�@��@���@���@�F@��@��@�?�@�'R@�	@���@��=@�_p@��@�u%@��@���@�_p@�Ɇ@�u@��6@��d@���@�s@�W?@��P@���@�Xy@�#:@�e@�@~�8@~($@}�S@} \@|ی@|�e@|V�@|%�@{��@{�	@z��@zGE@y��@y@x�j@xg8@xA�@w�@wy�@w6z@v�c@v�\@v.�@u}�@t>B@sn/@r�@r͟@r�!@r�r@rd�@q�)@qzx@qX@p�4@o@O@n��@n_@m�'@mQ�@m!�@l�E@lH@kݘ@ke�@j��@j�@j{�@j3�@i�T@i��@i:�@h��@h[�@g�Q@gy�@f�m@fa|@f+k@e�H@es�@d�5@c�A@cv`@cE9@b�c@b-@b �@a��@a�@a@`c�@_"�@^Z�@]�@]�~@]*0@\��@\G@[��@[e�@[P�@[J#@[J#@[�@Z\�@X�f@W��@V��@V� @V~�@VQ@U�H@U!�@TM@S�q@SE9@R�\@Q�z@QN<@P�e@O� @O)_@N�'@N��@N$�@N �@M�H@MVm@L�?@K�@KiD@K�@J��@Jq�@I�o@I��@IS&@I@@Hj@G��@G�V@G@O@F�6@E��@EDg@E@D�U@D�o@D!@C��@C��@C�{@CO@C)_@C�@C�@B�8@B��@B�@B��@Bv�@B1�@A�9@A�n@AB�@@��@@�@@e�@@9X@?�;@?|�@?�@>�@>��@>��@>_�@>5?@=��@=c@=0�@<ѷ@<��@<y>@<PH@<M@;�0@;F�@:�8@:��@:��@:v�@:_�@::*@:#:@:�@9�>@9��@9x�@9c�@94@8��@8�o@87�@7�@7�q@7�*@7K�@6�"@6��@6�<@6��@6�+@6)�@5�@5s�@5L�@4��@4�?@4�@4'R@3��@3�$@3s@3W?@3K�@3+@3�@2�2@2�R@2xl@2R�@2�@1��@1�'@1��@1w2@1S&@1*0@0��@0�@0 �@/�]@/�K@/�@.�h@.�L@.�r@.V@.H�@.B[@.;�@-�T@-�@-p�@-\�@-+�@,��@,�$@,�.@,[�@,�@+�@+�:@+�{@+Z�@++@+S@*�@*�@*�!@*�x@*�1@*�\@*Ov@*	@)�Z@)��@)��@)L�@(�9@(N�@($@(1@'��@'�@'>�@'!-@'�@&�H@&��@&kQ@&u@%�3@%��@%��@%��@%��@%%F@$�@$��@$]d@$,=@$1@#��@#�[@#�k@#t�@#�@"�6@"B[@"�@!�.@!��@!��@!�h@!x�@!hs@!N<@!q@ �@ ��@ �u@ U2@ �@� @�V@O@�@�@��@~�@W�@Q@Ov@0U@rG@Dg@?}@:�@%F@�@�@�Y@Xy@I�@>B@-�@'R@"h@�r@��@o�@;d@�@͟@�\@q�@Ta@+k@J@��@��@�@�"@�@�|@֡@�4@��@[�@6@�@@�@��@�[@�[@�	@]�@6z@!-@�@�H@��@�@H�@@��@S&@?}@&�@�E@�I@tT@1'@�g@�@��@dZ@�@��@��@kQ@M�@4@��@��@��@��@|@a�@O�@A @-w@�@�	@��@��@z�@M@�@�]@�@�&@��@��@�@��@�P@��@J#@�,@�h@�L@��@v�@p;@\�@C�@+k111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B�dB�]B	�B	�B	�B		�B	
XB	
=B	
XB	
�B	B	xB	�B�B	B	�B	�B	B	�B	OB	+B	AUB	IlB	LdB	V�B	f2B	jB	e�B	gmB	rB	��B	��B	�XB	�6B	āB	�1B	��B	�$B	֡B	�B	x�B	tB	��B	��B
[B
�B
abB
yXB)B��B��B��B��B�B��B�RB B��B��B�"B�B�B�SB�B��B��B�+BzBk�BbB\�BO�BA�B2�B�B<B
�qB
ߤB
֡B
��B
��B
�zB
|�B
d�B
D�B
/OB
$B
<B
9B	�MB	�IB	͹B	��B	�B	�lB	|�B	sMB	m]B	\]B	J�B	=qB	)yB	($B	(XB	>]B	4�B	*�B	(�B	#�B	!-B	#B	%�B	.�B	<B	2aB	,�B	'RB	%�B	"�B	 vB	0oB	D�B	WYB	k�B	s�B	��B	��B	��B	�B	��B	�?B	��B	��B	��B	��B	�NB	�TB	��B	��B	�B	��B	��B	��B	�B	�B	żB	�+B	ǔB	��B	�gB	�=B	��B	��B	��B	�B	�B	��B	��B	�B	�8B	��B	��B	�B	�_B	��B	�$B	�BB	�"B	�fB	�B	�\B	ݲB	�]B	רB	ңB	�	B	��B	�OB
B	��B	�B	�B	��B	��B	�B	��B	��B	�B	�B	�aB
�B
�B
B
#�B
"hB
# B
 �B
B
�B
B
	B
jB
)�B
4B
7LB
:�B
:�B
:�B
:^B
:�B
:�B
;B
<6B
<�B
<�B
;�B
:�B
9>B
9>B
9�B
9rB
8RB
6+B
2-B
)�B
$&B
#�B
�B
TB
BB
�B
B
�B
"B
�B
NB
B
�B
B
=B
1B
�B
FB
oB
uB
,B

B
�B
=B
�B
�B
oB
NB
�B
MB
_B
�B	��B	��B	�B	�qB	�B	��B	��B	��B	��B	��B	�eB	�B	�B	��B	�`B	�B	��B	�FB	�B	��B	��B	�B	�B	�nB	�hB	�-B	�B	�'B	��B	�VB	�jB	�B	ݘB	��B	�bB	�hB	�B	�B	�-B	��B	ߊB	�B	�=B	�KB	׍B	֡B	�B	�YB	ۦB	�B	�-B	��B	��B	��B	�B	�sB	�QB	�B	�TB	�B	�B	��B	�B	�GB	��B	�B	��B	��B	�tB	��B	�tB	�?B	�TB	�|B	��B	��B	�vB	�B	��B	��B	�cB	�B	�}B	�B	�B	�B	��B	�hB	�B	�B	��B	��B	�+B	�FB	�$B	��B	�DB	�B	�*B	�^B	�^B	��B	�JB	�PB	��B	�B	�(B	��B	��B
 B
�B
B
3B
�B
B
�B
B
B
�B
�B
�B
�B
YB
�B
%B
�B
�B
�B
�B
KB
�B
�B
�B
�B
fB
KB
KB
B
	B
	�B

�B

�B

�B

=B

#B
	�B
B
^B
�B
�B
�B
0B
dB
�B
PB
�B
�B
�B
<B
�B
�B
�B
�B
�B
.B
HB
.B
}B
}B
bB
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
\B
�B
bB
 B
hB
 B
 B
@B
&B
�B
�B
,B
B
FB
�B
�B
MB
MB
MB
B
�B
B
EB
_B
+B
�B
eB
�B
�B
�B
B
CB
xB
�B
�B
�B
�B
�B
IB
5B
B
OB
�B
B
B
pB
�B
�B
 BB
 �B
 �B
!-B
!-B
!bB
!bB
!HB
!bB
!|B
!|B
!bB
!|B
!�B
!�B
"4B
"B
"NB
"NB
"hB
"�B
"�B
#:B
#�B
$�B
$�B
$�B
%,B
$�B
%FB
%,B
&2B
'B
'�B
'�B
'�B
'�B
'�B
(sB
)*B
)B
)B
)*B
)*B
)B
)B
)�B
)_B
)�B
)�B
)yB
*B
*0B
*0B
)�B
*0B
*B
*�B
+B
+�B
,qB
,�B
,�B
,�B
-]B
-]B
-�B
.�B
/5B
/5B
/�B
0�B
0�B
1AB
1vB
2-B
1�B
1�B
1AB
1�B
1�B
1�B
2-B
2�B
2�B
3�B
3�B
4�B
5B
4�B
4�B
5%B
5�B
6B
6`B
6`B
6�B
6�B
6�B
6�B
72B
7fB
7�B
88B
8lB
8�B
8�B
8�B
9$B
9rB
9�B
9�B
9�B
9�B
:*B
:^B
;B
;dB
;B
;�B
;�B
;�B
;�B
<6B
<B
<6B
<jB
<�B
<�B
="B
=VB
=qB
=�B
>B
>B
>�B
>�B
?HB
?}B
?�B
?�B
?�B
@ B
@OB
@�B
@�B
@�B
A;B
A�B
A�B
B'B
BB
B�B
C�B
C�B
DB
DB
D�B
D�B
D�B
EB
ESB
E�B
GEB
G�B
HKB
HfB
H�B
H�B
I7B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
K�B
LJB
LdB
LJB
LdB
L�B
MB
MjB
NB
NB
N�B
OBB
O\B
O�B
PbB
P�B
QB
QB
QNB
QNB
QhB
Q�B
Q�B
R:B
R�B
R�B
SB
SB
S[B
SuB
S�B
TB
T�B
T�B
T�B
U2B
U�B
VSB
V�B
V�B
V�B
V�B
W?B
W?B
W?B
WYB
W�B
W�B
W�B
W�B
W�B
W�B
XB
XEB
X_B
XyB
X�B
X�B
YB
YKB
Y�B
Y�B
Y�B
ZB
Z7B
ZkB
ZkB
ZQB
Z�B
Z�B
Z�B
Z�B
[#B
[	B
[	B
[�B
[�B
\B
\)B
\xB
\�B
]IB
]~B
]~B
]~B
]~B
]�B
]�B
]�B
]�B
^B
^B
^jB
^�B
_B
_B
_;B
_VB
_pB
_VB
_�B
_�B
`BB
`BB
`\B
`vB
`�B
aB
a-B
abB
a�B
a�B
a�B
a�B
b4B
b4B
b4B
bNB
bNB
b�B
b�B
b�B
b�B
b�B
cB
cTB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
dtB
dtB
dtB
eFB
ezB
ezB
e�B
e�B
e�B
e�B
e�B
fB
fLB
fLB
ffB
f�B
f�B
f�B
gB
gB
g�B
g�B
g�B
g�B
g�B
h
B
h$B
h$B
hXB
hsB
hsB
hsB
hsB
h�B
h�B
h�B
i*B
iB
i*B
i�B
jB
j0B
jKB
j0B
j�B
kB
kB
kB
k6B
kB
k�B
k�B
l"B
l=B
l=B
l"B
l"B
l�B
l�B
m)B
mCB
mwB
mwB
mwB
m�B
m�B
m�B
m�B
n}B
o B
oB
oB
o5B
o5B
oOB
oOB
oiB
oiB
o�B
pB
p!B
p!B
pUB
p�B
p�B
p�B
qAB
qvB
q�B
q�B
rB
r-B
rB
q�B
q�B
sB
r�B
r�B
r�B
sB
r�B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tnB
t�B
t�B
uB
u?B
u?B
utB
u�B
u�B
u�B
v+B
v+B
vFB
v�B
v�B
v�B
w2B
wLB
w�B
w�B
w�B
w�B
w�B
xB
xB
xB
xB
xB
xRB
xlB
xlB
x�B
x�B
x�B
y	B
y>B
y�B
y�B
y�B
zB
zxB
z�B
z�B
{0B
{dB
{dB
{B
{�B
|6B
|�B
|�B
|�B
|�B
}"B
}<B
}"B
}<B
}qB
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~B
~BB
~wB
~wB
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
B
}B
}B
�B
�B
�B
�B
�B
�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�xB�^B��B�B��B	GB	YB	KB		�B	
rB	
=B	
rB	
�B	)B	�B	�B�_B	�B	�B	�B	B	�B	OB	+B	A;B	IlB	L~B	V�B	f�B	jB	e�B	gRB	q�B	��B	��B	�>B	�B	�gB	�fB	��B	��B	�B	��B	{B	yXB	�B	�gB
�B
KB
g�B
�'B&B�6B��B�iB��B�B�[B��B�B�BAB �B�<B�B��B�VB��B��B�XB}qBmwBc�B`BR�BE9B6B�B BuB
�B
چB
�HB
��B
�#B
��B
i�B
J#B
3hB
�B
}B
�B	��B	��B	�oB	� B	��B	��B	�B	v�B	qvB	`\B	O(B	@�B	+B	*�B	,�B	B�B	8RB	,qB	*B	&�B	$B	$tB	(>B	33B	>�B	4B	/B	)DB	'mB	$�B	# B	1�B	ESB	W�B	l"B	uZB	�%B	��B	��B	�_B	��B	��B	�+B	�B	��B	��B	�B	�uB	��B	��B	��B	��B	��B	�fB	��B	��B	��B	�EB	��B	�XB	�B	��B	ݘB	�B	��B	�B	�wB	�B	�B	�`B	�	B	��B	��B	��B	�B	�_B	��B	��B	�]B	�B	�:B	�|B	��B	ݲB	�B	��B	�^B	�JB	�B
9B
 �B	��B	�B	�RB	��B	�B	��B	�wB	�B	�B	�'B
B
�B
;B
$@B
"�B
#�B
!�B
B
�B
+B
�B
�B
)_B
4B
7�B
;B
:�B
:�B
:�B
;B
;�B
<B
<�B
=VB
<�B
<PB
:�B
9rB
9rB
:*B
:B
9$B
72B
3MB
*�B
$�B
$�B
�B
&B
.B
\B
(B
}B
�B
�B
hB
yB
 BB
B
�B
7B
�B
�B
oB
�B
aB
?B
KB
B
�B
{B
&B
�B
	B
MB
1B
	�B	��B	��B	�^B	�]B	��B	��B	��B	��B	��B	�[B	�6B	��B	��B	�2B	��B	�`B	�LB	��B	��B	��B	��B	��B	�ZB	��B	�B	�HB	��B	�'B	�'B	߾B	��B	�IB	��B	�BB	�|B	�B	�B	�NB	��B	��B	��B	�~B	ۦB	ٴB	��B	��B	�mB	��B	��B	�B	�HB	�B	��B	�
B	��B	�B	�B	�-B	��B	��B	��B	�MB	�MB	�B	�aB	�B	�B	�B	��B	��B	��B	��B	��B	��B	�aB	�-B	��B	�oB	�IB	�}B	�B	��B	��B	� B	�B	��B	��B	�B	��B	��B	�B	�B	�zB	�B	��B	�*B	�^B	�*B	�^B	�xB	�xB	��B	�B	�jB	��B	�]B	�wB	�BB	�.B
;B
�B
aB
gB
B
SB
�B
B
9B
�B
�B
+B
+B
�B
B
tB
EB
B
�B
B
�B
�B
�B
�B
�B
fB
fB
�B
�B
	7B
	�B

�B
B

�B

=B

XB

=B
xB
^B
�B
�B
�B
dB
�B
6B
�B
�B
�B
�B
�B
�B
�B
(B
�B
B
bB
}B
bB
�B
�B
}B
�B
�B
�B
B
TB
�B
�B
�B
�B
�B
�B
�B
�B
B
\B
�B
B
�B
NB
�B
TB
�B
uB
@B
�B
�B
FB
FB
�B
�B
B
MB
gB
�B
mB
�B
+B
�B
�B
yB
�B
�B
B
#B
�B
B
]B
xB
�B
�B
�B
�B
/B
~B
jB
OB
jB
�B
;B
;B
�B
�B
�B
 \B
 �B
 �B
!HB
!HB
!bB
!bB
!bB
!|B
!�B
!|B
!bB
!�B
"B
"4B
"4B
"4B
"�B
"hB
"�B
"�B
"�B
#�B
$B
$�B
$�B
%B
%,B
%B
%�B
%�B
&�B
'RB
(
B
'�B
'�B
(
B
(
B
(�B
)*B
)*B
)*B
)*B
)*B
)*B
)DB
)�B
)�B
)�B
)�B
)�B
*B
*B
*eB
*0B
*B
*KB
*�B
+QB
,"B
,�B
-B
,�B
-B
-�B
-�B
.IB
.�B
/5B
/iB
/�B
0�B
1B
1vB
1�B
2aB
1�B
2aB
1�B
2B
1�B
1�B
2GB
2�B
33B
3�B
3�B
4�B
5%B
5%B
5?B
5tB
5�B
6FB
6zB
6`B
6�B
6�B
6�B
6�B
7LB
7�B
8B
8lB
8�B
8�B
8�B
9	B
9XB
9�B
9�B
9�B
9�B
:B
:�B
:�B
;0B
;dB
;�B
;�B
;�B
;�B
<B
<PB
<PB
<�B
<�B
<�B
="B
="B
=qB
=�B
=�B
>BB
>BB
>�B
?B
?HB
?�B
?�B
?�B
?�B
@B
@iB
@�B
@�B
AB
AUB
A�B
BB
BAB
B'B
B�B
C�B
DB
DMB
D3B
D�B
D�B
EB
E9B
E�B
F%B
G�B
HB
HfB
H�B
H�B
H�B
IlB
I�B
I�B
I�B
I�B
I�B
I�B
JXB
K^B
K�B
LdB
L~B
LJB
L�B
L�B
MjB
M�B
N"B
NVB
N�B
OvB
O�B
PB
P�B
Q B
QB
Q4B
QhB
QhB
Q�B
Q�B
R B
RTB
R�B
S&B
SB
S&B
SuB
S�B
TB
TFB
T�B
T�B
U2B
U�B
U�B
VmB
V�B
V�B
V�B
V�B
W?B
WYB
WYB
WsB
W�B
W�B
W�B
W�B
W�B
W�B
XB
XEB
XyB
XyB
X�B
X�B
Y1B
YKB
Y�B
Y�B
Y�B
Z7B
ZQB
Z�B
ZkB
ZQB
Z�B
Z�B
Z�B
[	B
[=B
[	B
[#B
[�B
[�B
\B
\]B
\�B
\�B
]IB
]~B
]dB
]�B
]�B
]�B
]�B
]�B
]�B
^B
^5B
^jB
^�B
_!B
_!B
_VB
_VB
_pB
_pB
_�B
_�B
`BB
`\B
`\B
`�B
`�B
a-B
aHB
a|B
a�B
a�B
a�B
bB
b4B
bNB
bNB
bNB
bhB
b�B
b�B
b�B
b�B
b�B
cB
cnB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d@B
d�B
d�B
d�B
eFB
ezB
e�B
e�B
e�B
e�B
e�B
e�B
fB
fLB
ffB
f�B
f�B
f�B
f�B
gB
g8B
g�B
g�B
g�B
g�B
g�B
h$B
h>B
h>B
hXB
hXB
hXB
hsB
hsB
h�B
h�B
iB
iDB
iB
i_B
i�B
jB
jKB
jeB
jKB
j�B
kB
kB
kB
kQB
k6B
k�B
k�B
l"B
l=B
l=B
l=B
l"B
l�B
l�B
mCB
m]B
mwB
mwB
mwB
m�B
m�B
m�B
nB
n}B
oB
o5B
o5B
o5B
oOB
oOB
o5B
o�B
o�B
o�B
p!B
p!B
p;B
poB
p�B
p�B
qB
q[B
q�B
q�B
q�B
r-B
r-B
rB
q�B
r-B
sB
r�B
r�B
r�B
s3B
sB
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tB
t�B
t�B
t�B
u%B
u?B
uZB
u�B
u�B
u�B
u�B
vFB
v+B
v`B
v�B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
x8B
xRB
xlB
x�B
x�B
x�B
x�B
y	B
yXB
y�B
y�B
zB
z*B
zxB
z�B
z�B
{0B
{B
{B
{�B
{�B
|6B
|�B
|�B
|�B
|�B
}<B
}"B
}"B
}VB
}�B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~(B
~(B
~]B
~wB
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
B
�B
cB
�B
�B
�B
�B
�B
�B
�311111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<9#�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.07(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201906030046132019060300461320190603004613202207271131152022072711311520220727113115202207271533582022072715335820220727153358  JA  ARFMdecpA30a                                                                20190523093710  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190523093737  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190523093737  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190523093738  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190523093738  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190523093739                      G�O�G�O�G�O�                JA  ARUP                                                                        20190523125705                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190524000000  CF  PSAL_ADJUSTED_QC@�B�G�O�                JM  ARCAJMQC2.0                                                                 20190602154613  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190602154613  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023115  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063358  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081508                      G�O�G�O�G�O�                