CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-08-11T09:37:23Z creation;2019-08-11T09:37:25Z conversion to V3.1;2022-08-02T05:12:15Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190811093723  20220818081508  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_019                    2C  D   APEX                            8420                            2.11.2                          846 @���#�
 1   @���|5�@,c�����c����1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A��A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B7��B@��BH  BO33BX  B`  Bh  Bp  Bx  B�33B�  B���B�  B�  B�  B�  B�  B�33B���B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33Bߙ�B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C�C�fC�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C033C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�C3DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D���D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�G�@��A�A"�RA@��AaA���A���A���A�z�A�Q�A�Q�A��A�z�B =qBG�B\)BG�B G�B(G�B0\)B8
=BA(�BH(�BOQ�BX33B`Q�Bh=qBpQ�BxQ�B�aHB�\B�B��B��B�(�B�(�B�#�B�k�B�B�33B��fB��
B�\B��B�
=B��B�.B�.B��B��B��B�(�B�\)B߳3B�{B�(�B�(�B�#�B�#�B�.B�#�C \C\C�C{C
C
{C�C5�C!HC�C�qC�C�C
C�C�C �C"�C$�C&�C(�C*\C,\C.�C0O\C2�C4
=C6\C8\C:
=C<�C>\C@�CB�CD�CF�CH
=CJ�CL�CN�CP
=CR
=CT
CV�CX)CZ�C\\C^�C`
Cb0�Cd{Cf
=Ch{Cj
Cl{Cn�Cp�Cr{Ct\Cv
=Cx\Cz
C|{C~\C�fC�C��C�C�fC��C��C��C��C��C��C�fC�fC�C��C��C��C�
=C��C�
=C��C�fC��C�
=C�C�fC��C��C��C�fC�fC��C�C�C��C��C��C�
=C��C��C�
=C��C�
=C�
=C��C�
=C��C�fC��C��C��C��C�fC��C�
=C��C��C��C��C�fC��C��C��C��C��C�
=C�fC��C��C��C�
=C�C��C��C��C��C��C��C�C��C�C��C��C��C�
=C��C��C��C�fC�fC�C��C��C�C��C��C�C�
=C��C�C��C�fC��C�
=C��C��C��C�
=C�C��C�C��C��C��C��C��C��C��C�fC��C��C��C�
=C�
=C�
=C�
=C�
=C��D 3D �3D{D�{D3D��D3D��D�D��DD��D�D��D
D�{D�D��D	3D	��D
�D
��D�D�HD�D��DfD�fD
D�fDD�3D�D�3D�D��D�D�{D�D�3D3D�3DD�D{D��D�D��D�D��D�D��D�D��D{D�{D�D��D{D�3D�D�D{D�
D RD �{D!�D!��D"D"�{D#3D#�{D$�D$�{D%{D%�3D&�D&�D'{D'��D(3D(�{D)D)�3D*�D*��D+�D+��D,�D,�D-�D-��D.3D.��D/�D/�{D03D0��D13D1�{D23D2�3D33D3��D4 �D4��D5{D5�D63D6��D7�D7�3D83D8��D9{D9�D:fD:�{D;�D;�{D<D<��D=�D=��D>�D>�{D?fD?�D@�D@�3DA3DA�DB{DB�{DC
DC�DD�DD�{DE�DE��DF�DF�{DG{DG�DH{DH�DIDI�DJ�DJ��DKRDK��DL{DL�DM�DM��DN{DN�{DO{DO��DP�DP�3DQ�DQ�3DR{DR�{DS�DS�{DTDT�{DU�DU�3DV�DV�DW�DW��DX�DX��DYDY��DZ3DZ�D[fD[�
D\�D\��D]�D]��D^�D^��D_�D_�3D`D`�
Da{Da�3Db{Db��Dc{Dc�Dd�Dd�De{De��Df�Df��DgDg�{DhDh�fDi
Di�fDjfDj�fDk�Dk�fDl�Dl�Dm�Dm�{Dn3Dn�{Do�Do�{Dp�Dp�{Dq�Dq�3Dr{Dr�fDsDs��Dt{Dt�{Du{Du�{Dv{Dv�
Dw�Dw��DxfDx�{Dy{Dy��Dz�Dz��D{�D{��D|3D|�3D}�D}�{D~fD~��D�D��D�3D�B�D��=D���D�HD�AHD���D�D��D�A�D���D�D�3D�B�D���D���D��D�B�D���D��=D��D�B=D���D���D� �D�A�D���D��HD��D�B�D��=D���D��D�B=D��=D���D��D�B=D���D���D��D�B�D���D���D��D�C3D���D��=D��D�A�D���D�D��D�@�D��HD���D��D�AHD���D��=D��D�A�D���D��=D�3D�B�D���D��=D��D�B�D���D���D��D�B�D���D���D��D�B=D���D���D�=D�B�D��3D�ÅD�3D�A�D���D��3D��D�B�D��3D�D�=D�B=D���D��=D��D�B=D���D���D��D�A�D���D���D�=D�A�D��HD��=D�=D�A�D���D���D��D�B=D���D���D�=D�B�D���D��=D��D�B�D���D�D��D�A�D���D���D��D�A�D��=D��=D�=D�AHD��HD��HD�HD�A�D���D��=D�=D�B�D���D��3D��D�A�D���D�ÅD��D�B=D���D���D�HD�@�D���D���D�HD�A�D��=D���D�HD�B�D��=D���D�HD�B=D��3D���D��D�B�D���D���D�=D�B�D��HD��=D�=D�B=D���D��HD��D�B�D���D��3D�=D�B�D��3D��=D��D�B=D���D���D�=D�B=D���D���D��D�A�D���D���D��D�B=D���D���D��D�B=D���D���D��D�B=D���D���D��D�A�D��=D���D��D�A�D���D���D�HD�A�D��=D���D��D�B�D��=D��=D��D�A�D���D���D�3D�B=D���D���D��D�C3D���D���D��D�AHD��HD���D� �D�A�D���D��RD� �D�A�D���D��=D� �D�@�D��3D���D��D�C3D��3D���D��D�A�D��HD��=D��D�C�D�D��=D��D�@�DÂ�D���D�3D�D)DĂ�D��HD��D�B=Dł�D���D�=D�B�DƂ�D��=D��D�A�DǁHD��HD��D�B=DȂ=D���D�3D�B=DɁ�D���D��D�B=Dʂ=D���D�=D�A�D˂=D���D�3D�A�D́�D��=D�=D�B=D́�D���D��D�A�D΂�D���D��D�B�Dς�D�D��D�B�DЁHD���D� RD�B=Dт�D��HD��D�B�Dҁ�D���D��D�C�Dӂ=D���D� �D�A�Dԃ3D��=D��D�C3DՂ�D��=D�=D�B�Dց�D�D�=D�B=Dׂ=D���D�HD�AHD؁�D���D��D�B=DفHD���D��D�B�Dڂ=D��HD��D�B=Dہ�D��=D�3D�B�D܂�D���D��D�A�D݂�D���D��D�B�Dނ=D���D��D�A�D߂=D��=D��D�B=D��HD��=D��D�A�D��D���D��D�B=D��D�D�=D�B�D�3D���D��D�A�D䁚D���D��D�B�D�HD���D��D�B=D��D���D��D�A�D��D���D��D�A�D�=D���D�=D�C3D�=D��HD��D�A�D��D�D�3D�B�D��D�D�=D�B=D�=D��=D�3D�C3D�=D��=D��D�A�DD���D�=D�B�D��D���D�=D�C3D���D��HD� �D�A�D�D��=D��D�C3D�=D���D��D�C3D��D��HD��D�A�D�D���D��D�B�D��=D���D��D�B�D��3D���D�HD�@�D���D���D��D�B�D���D���D��D�A�D���D��3D�=D�AHD��HD�D��D�C�D�o\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9�A�?�A�B�A�E9A�C-A�CaA�FA�FA�G�A�IA�K^A�J�A�J�A�J#A�H�A�9�A�/OA�"4A�A��dA�wA�[#A��WA�R A� 'AךkA�H�A�4�A�e,A�H�A��A���A�Q�Aɻ0Aɐ�A�JXA�%�A�PHA�#�A�_pA��AÈ1AeA���A���A��QA�J�A��3A�kA���A�1�A�c�A��5A��A�y�A�$�A�'�A���A��"A�ޞA���A�@�A��A��'A��A��A�PA��A��A���A�x�A�}�A���A�k�A�z�A�-A���A��A�iDA�{�A���A�A� �A��A��A}8A{K�AxOAtqvApl�Am/�Aj�SAg�AcW�A` iA]Z�AZ�AV��AS�AO�ANR�AJ?}AH�sAHYAF=AB�UAAB[A?a|A= iA:f�A8|�A7!�A5��A3�yA21A/;dA.��A.W?A-�A,_A*�IA(�pA'j�A&A$�EA$e�A#�A"P�A!�"A!��A!�?A!xlA!0�A ںA !A ��A#$�A!�PA �AIRA��A($A iA�$A�AzxA��A�rAH�Ac AffA�@A$tAZAYKAsA �A�MA��AJ�A�A�UA�6A6�A��AS&A��AXA  Ad�A��A�Aq�A�A��A��A�Ai�AGAm]A�SAMA��A;A
1'A�A��A��A��A;AX�A��A�rAg8Aa|Ah�A��A��A͟AdZA�nAqA|�A.�A��As�AeA��A��A�;A��Ag�AE9A?AYAGA ��A �9A ��A ��A XA $t@��r@���@�"�@�e�@���@���@�s�@��~@��@��z@��^@�J�@��@���@��@�@�m]@�a�@�S�@�$t@�6@�~�@�%�@��@�u%@�W?@�e@��@�f@���@��[@�{�@�o@� @�u@�j@��@镁@�@�M@��@���@��@�q@�}V@���@�`B@��@�@��@���@�@޼j@ޫ6@ޕ�@�v�@޹$@�6�@ݖS@�?}@��@�2a@ܶ�@���@��@پw@��c@��)@׸�@��@ֈ�@��B@֙1@�]d@��T@Ե@�u@ӊ�@ұ�@�)�@��g@�v`@��@�_�@�m]@��@Υz@�[�@��@͊	@�A�@��`@�M@�c@�9�@��@�3�@ɚk@�RT@��E@� �@ǩ�@ǀ4@ǿH@���@���@�/�@�)�@��/@�|@��@��8@��@F@�$�@��#@�#�@��@�W�@��A@���@�t�@�F@�H@��-@���@���@�l�@�+�@��@���@��X@�kQ@���@�&@��.@�R�@��@���@�/�@�
=@���@�_@��6@���@�F@�%@�ں@���@���@�r�@���@���@��@��1@���@�)_@��	@��@��@�z@�$@���@���@���@���@�e�@�>�@��r@�GE@���@��@�]�@�?}@�;@�h�@��^@�X@�J#@��@�c @�<�@�J@��{@�,�@�C@�"�@�@��@���@���@���@�W?@�Q�@�
�@���@���@�>�@��'@�;�@���@��N@�!-@�tT@�S�@�(�@�O@�x@� �@��@���@�b�@�4�@��@��@��o@�9X@���@��8@���@�\�@�e@���@��*@���@�Dg@��@��@���@��I@�0U@��
@�zx@�+�@��@�ߤ@�u%@��@��@�J�@��"@���@��]@���@���@�h�@�b@���@��M@�>�@��@��U@��@�m�@�-@��@��@���@��@���@���@�N�@��@���@���@�g�@�8�@���@�Ft@�1@���@��$@�,�@���@��/@��'@��@�B[@��@��@��V@��:@�k�@�ں@��\@�L0@�#:@�1@���@���@��@�]d@�
�@��
@��h@�Q�@�C@���@��p@���@���@��@��n@�@O@��[@�v�@�H�@�#:@��@�dZ@��@���@���@��z@�e,@�+�@��P@��@�Ĝ@�Xy@�'R@��N@��:@�@@���@��o@�c�@�	@�9�@��)@��\@�e�@�$�@���@��k@���@�RT@�'�@��@��f@��`@���@��r@�YK@�$�@��@��@a@~ں@~z@~Q@~@}}�@|�f@|@{�@{��@{��@{6z@{&@{�@za|@zGE@z=q@y�@y��@y`B@x�v@xPH@w��@w1�@v�m@v��@v�\@u��@u^�@u*0@te�@t4n@s�@s��@s,�@r��@r�@q��@q \@p�Y@p!@o�Q@o>�@m�.@ms�@l֡@l!@k�
@k��@kX�@j�6@j@�@i|@h��@h1@g@O@f͟@f�!@f)�@eS&@e@d�@d�4@dC-@c��@ca@c�@b��@b5?@a�z@a�^@a�S@a�@`l"@`"h@_��@_�a@_g�@^z@\�K@\m�@\U2@[�@Z��@Y��@Y+�@X�$@X�_@X[�@W��@W�k@WiD@W'�@V�'@U��@Uj@U	l@TU2@Sy�@S�@R��@R��@R4@Q��@Q[W@P��@P��@P?�@O��@Oƨ@O��@O'�@N�X@N�1@N��@Nff@M��@M��@MG�@M�@L��@L�[@L�p@L�j@L�@LtT@K�@K��@Kj�@KP�@Jߤ@J��@Jv�@JV@J-@I��@I�'@I8�@HɆ@H�_@Hl"@H1@G~�@F��@F�@E��@EVm@D��@D@Ce�@B�c@B��@A�@A`B@@��@@r�@@*�@?�F@?t�@>�2@>��@>s�@=��@=A @<ѷ@<r�@;��@;�4@;C@;S@:�M@:�R@:V@:1�@:�@:u@9��@9+@8�)@8�o@8j@8]d@8Q�@8�@7�@7>�@7S@6͟@6�x@6l�@6{@5�@5�@5}�@5j@5@4��@4_@3��@3�q@3b�@3H�@3�@2�s@2kQ@2�@1�@1c@0��@0w�@0c�@0_@/� @/J#@/S@.��@.O@-�@-��@-k�@-Y�@-IR@-@,�j@,��@,�Y@,Z@+�+@+��@+��@+s@+8@+33@+,�@*�@*�R@*V@*$�@)�#@)�~@)=�@(��@(�U@(��@(��@(��@(�@(r�@(<�@'�@'��@'X�@'1�@'C@&�'@&�6@&}V@&@�@&&�@&	@%��@%�7@%T�@%Dg@$�@$��@$1'@#��@#�@#y�@"�8@"� @"Ta@!�@!IR@ ��@ tT@ g8@ 1@ݘ@�V@J#@(@�H@��@{@��@�@�7@X@2a@��@��@q@1'@��@�V@O@9�@�@�,@q�@�@��@�@c@4@�@��@�)@��@�@_@ �@��@˒@��@U�@�@�@�A@v�@u%@8�@�D@�C@p�@f�@ \@��@��@K^@  @�@ƨ@�@@o�@X�@O@33@&@Y@ i@��@��@�@��@i�@M�@;�@($@{@	@@�@��@�t@��@�^@m]@Dg@8�@+�@�P@�v@�j@��@�_@oi@PH@/�@!@x@�:@8@!-@@(@�2@�h@�1@l�@H�@3�@�@�@�9@�d@��@��@�M@`B@G�@�@�K@�[@��@��@>B@�W@خ@s@a@H�@�@
�<@
�@
��@
E�@
-@
@
J@

�@
�@
�@	�@	��@	�t@	��@	o @	L�@��@�$@��@w�@q@oi@e�@/�@�A@�}@��@��@j�@9�@�@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�9�A�?�A�B�A�E9A�C-A�CaA�FA�FA�G�A�IA�K^A�J�A�J�A�J#A�H�A�9�A�/OA�"4A�A��dA�wA�[#A��WA�R A� 'AךkA�H�A�4�A�e,A�H�A��A���A�Q�Aɻ0Aɐ�A�JXA�%�A�PHA�#�A�_pA��AÈ1AeA���A���A��QA�J�A��3A�kA���A�1�A�c�A��5A��A�y�A�$�A�'�A���A��"A�ޞA���A�@�A��A��'A��A��A�PA��A��A���A�x�A�}�A���A�k�A�z�A�-A���A��A�iDA�{�A���A�A� �A��A��A}8A{K�AxOAtqvApl�Am/�Aj�SAg�AcW�A` iA]Z�AZ�AV��AS�AO�ANR�AJ?}AH�sAHYAF=AB�UAAB[A?a|A= iA:f�A8|�A7!�A5��A3�yA21A/;dA.��A.W?A-�A,_A*�IA(�pA'j�A&A$�EA$e�A#�A"P�A!�"A!��A!�?A!xlA!0�A ںA !A ��A#$�A!�PA �AIRA��A($A iA�$A�AzxA��A�rAH�Ac AffA�@A$tAZAYKAsA �A�MA��AJ�A�A�UA�6A6�A��AS&A��AXA  Ad�A��A�Aq�A�A��A��A�Ai�AGAm]A�SAMA��A;A
1'A�A��A��A��A;AX�A��A�rAg8Aa|Ah�A��A��A͟AdZA�nAqA|�A.�A��As�AeA��A��A�;A��Ag�AE9A?AYAGA ��A �9A ��A ��A XA $t@��r@���@�"�@�e�@���@���@�s�@��~@��@��z@��^@�J�@��@���@��@�@�m]@�a�@�S�@�$t@�6@�~�@�%�@��@�u%@�W?@�e@��@�f@���@��[@�{�@�o@� @�u@�j@��@镁@�@�M@��@���@��@�q@�}V@���@�`B@��@�@��@���@�@޼j@ޫ6@ޕ�@�v�@޹$@�6�@ݖS@�?}@��@�2a@ܶ�@���@��@پw@��c@��)@׸�@��@ֈ�@��B@֙1@�]d@��T@Ե@�u@ӊ�@ұ�@�)�@��g@�v`@��@�_�@�m]@��@Υz@�[�@��@͊	@�A�@��`@�M@�c@�9�@��@�3�@ɚk@�RT@��E@� �@ǩ�@ǀ4@ǿH@���@���@�/�@�)�@��/@�|@��@��8@��@F@�$�@��#@�#�@��@�W�@��A@���@�t�@�F@�H@��-@���@���@�l�@�+�@��@���@��X@�kQ@���@�&@��.@�R�@��@���@�/�@�
=@���@�_@��6@���@�F@�%@�ں@���@���@�r�@���@���@��@��1@���@�)_@��	@��@��@�z@�$@���@���@���@���@�e�@�>�@��r@�GE@���@��@�]�@�?}@�;@�h�@��^@�X@�J#@��@�c @�<�@�J@��{@�,�@�C@�"�@�@��@���@���@���@�W?@�Q�@�
�@���@���@�>�@��'@�;�@���@��N@�!-@�tT@�S�@�(�@�O@�x@� �@��@���@�b�@�4�@��@��@��o@�9X@���@��8@���@�\�@�e@���@��*@���@�Dg@��@��@���@��I@�0U@��
@�zx@�+�@��@�ߤ@�u%@��@��@�J�@��"@���@��]@���@���@�h�@�b@���@��M@�>�@��@��U@��@�m�@�-@��@��@���@��@���@���@�N�@��@���@���@�g�@�8�@���@�Ft@�1@���@��$@�,�@���@��/@��'@��@�B[@��@��@��V@��:@�k�@�ں@��\@�L0@�#:@�1@���@���@��@�]d@�
�@��
@��h@�Q�@�C@���@��p@���@���@��@��n@�@O@��[@�v�@�H�@�#:@��@�dZ@��@���@���@��z@�e,@�+�@��P@��@�Ĝ@�Xy@�'R@��N@��:@�@@���@��o@�c�@�	@�9�@��)@��\@�e�@�$�@���@��k@���@�RT@�'�@��@��f@��`@���@��r@�YK@�$�@��@��@a@~ں@~z@~Q@~@}}�@|�f@|@{�@{��@{��@{6z@{&@{�@za|@zGE@z=q@y�@y��@y`B@x�v@xPH@w��@w1�@v�m@v��@v�\@u��@u^�@u*0@te�@t4n@s�@s��@s,�@r��@r�@q��@q \@p�Y@p!@o�Q@o>�@m�.@ms�@l֡@l!@k�
@k��@kX�@j�6@j@�@i|@h��@h1@g@O@f͟@f�!@f)�@eS&@e@d�@d�4@dC-@c��@ca@c�@b��@b5?@a�z@a�^@a�S@a�@`l"@`"h@_��@_�a@_g�@^z@\�K@\m�@\U2@[�@Z��@Y��@Y+�@X�$@X�_@X[�@W��@W�k@WiD@W'�@V�'@U��@Uj@U	l@TU2@Sy�@S�@R��@R��@R4@Q��@Q[W@P��@P��@P?�@O��@Oƨ@O��@O'�@N�X@N�1@N��@Nff@M��@M��@MG�@M�@L��@L�[@L�p@L�j@L�@LtT@K�@K��@Kj�@KP�@Jߤ@J��@Jv�@JV@J-@I��@I�'@I8�@HɆ@H�_@Hl"@H1@G~�@F��@F�@E��@EVm@D��@D@Ce�@B�c@B��@A�@A`B@@��@@r�@@*�@?�F@?t�@>�2@>��@>s�@=��@=A @<ѷ@<r�@;��@;�4@;C@;S@:�M@:�R@:V@:1�@:�@:u@9��@9+@8�)@8�o@8j@8]d@8Q�@8�@7�@7>�@7S@6͟@6�x@6l�@6{@5�@5�@5}�@5j@5@4��@4_@3��@3�q@3b�@3H�@3�@2�s@2kQ@2�@1�@1c@0��@0w�@0c�@0_@/� @/J#@/S@.��@.O@-�@-��@-k�@-Y�@-IR@-@,�j@,��@,�Y@,Z@+�+@+��@+��@+s@+8@+33@+,�@*�@*�R@*V@*$�@)�#@)�~@)=�@(��@(�U@(��@(��@(��@(�@(r�@(<�@'�@'��@'X�@'1�@'C@&�'@&�6@&}V@&@�@&&�@&	@%��@%�7@%T�@%Dg@$�@$��@$1'@#��@#�@#y�@"�8@"� @"Ta@!�@!IR@ ��@ tT@ g8@ 1@ݘ@�V@J#@(@�H@��@{@��@�@�7@X@2a@��@��@q@1'@��@�V@O@9�@�@�,@q�@�@��@�@c@4@�@��@�)@��@�@_@ �@��@˒@��@U�@�@�@�A@v�@u%@8�@�D@�C@p�@f�@ \@��@��@K^@  @�@ƨ@�@@o�@X�@O@33@&@Y@ i@��@��@�@��@i�@M�@;�@($@{@	@@�@��@�t@��@�^@m]@Dg@8�@+�@�P@�v@�j@��@�_@oi@PH@/�@!@x@�:@8@!-@@(@�2@�h@�1@l�@H�@3�@�@�@�9@�d@��@��@�M@`B@G�@�@�K@�[@��@��@>B@�W@خ@s@a@H�@�@
�<@
�@
��@
E�@
-@
@
J@

�@
�@
�@	�@	��@	�t@	��@	o @	L�@��@�$@��@w�@q@oi@e�@/�@�A@�}@��@��@j�@9�@�@1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	*eB	*KB	*B	*�B	*KB	)�B	)�B	)�B	)�B	)�B	*KB	)�B	)�B	*KB	+B	+�B	+�B	+B	*�B	)yB	'�B	&2B	%�B	U�B	t�B	~wB	tTB	w�B	��B	�KB	�B	��B	�nB
?�B
��B
�B
�JB
��B
��B
�zB
�B
��B
�[B�B-�B%,B$@B/OB<�BL�BP�BPHBEmBDMBF�B=qB="BG�BU�Bm�BhsBl�B}qB|Bz�B�%Bv+B{JBt�Bf�BmBE�B9B
�B
�,B
�B
�B
�6B
B
�=B
�}B
qB
B�B
�B	�B	��B	��B	��B	��B	��B	��B	�iB	l�B	V�B	CB	7B	,qB	�B	�B	�B		7B��B�`B��B��B�&B�jB��B�=BܒB�B�BߤBیB�aBҽBԕB��B�SB��B��B�B�/B�hB��B��B�	B�B��B	jB	�B	�B	!HB	&�B	-CB	B�B	�YB	|�B	��B	}�B	y$B	��B	��B	��B	��B	��B	��B	�gB	��B	�#B	�;B	��B	�KB	�'B	�:B	��B	�0B	��B	�QB	��B	�
B	�8B	�nB	��B	�[B	�B	ɺB	�XB	��B	��B	�#B	��B	�xB	�vB	�DB	��B	�hB	�B	߾B	߾B	�xB	�bB	��B	��B	�B	��B	҉B	ؓB	��B	��B	��B	ޞB	��B	��B	��B	��B	�5B	�DB	��B	�9B	�B	�B	�/B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�BB	�B	�BB	��B	�HB	�B	��B	�B	��B	��B	�B	��B	�B	�VB	��B	��B	��B	�B	�TB	�ZB	��B	�B	��B	�DB	�PB	�<B	��B	�0B	��B	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	�2B	�+B	�+B	�8B	��B	��B	�rB	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�_B	�B	�oB	�!B	�B	�B	�B	��B	�B	��B	�XB	�LB	�B	�B	�B	�B	�oB	�B	�B	�OB	�B	�B	�B	�RB	�`B	�@B	��B	�B	�B	��B	��B	��B	��B	��B	�8B	�B	�,B	��B	�TB	��B	�B	�LB	�RB	�B	�$B	��B	�B	�QB	�B	�B	�GB	�B	�;B	�cB	��B	�WB	�B	�B	��B	�B	��B	�B	� B	�}B	�B	�B	�iB	�B	�iB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�nB	�ZB	��B	�B	��B	�LB	��B	��B	�B	��B	��B	�$B	��B	�B	�jB
 �B
�B
SB
�B
�B
�B
GB
-B
�B
�B
�B
�B
MB
3B
�B
B
?B
�B
�B
?B
tB
?B
tB
EB
�B
_B
�B
+B
_B
_B
�B
�B
	B
	lB
	�B

=B

XB

XB

�B
DB
DB
B
PB
�B
"B
BB
�B
B
�B
�B
�B
�B
�B
 B
 B
:B
:B
:B
�B
uB
�B
�B
�B
,B
,B
�B
gB
�B
B
SB
�B

B
�B

B
YB
sB
�B
�B
�B
_B
�B
1B
eB
B
KB
KB
�B
QB
�B
	B
#B
=B
WB
�B
CB
xB
~B
�B
�B
B
�B
OB
�B
jB
�B
!B
VB
;B
VB
�B
 �B
 vB
 �B
 vB
 BB
!B
!-B
!|B
!bB
"B
"hB
"hB
"hB
"�B
#:B
#�B
$B
%�B
%�B
%�B
%�B
'�B
(sB
(XB
(�B
(�B
(�B
)B
(�B
(�B
)_B
)B
)*B
)_B
)�B
)�B
*B
)�B
*�B
*B
*eB
*�B
+�B
+�B
+�B
,B
,WB
-B
-]B
-�B
/B
/OB
/iB
/�B
0!B
0�B
0�B
0;B
/�B
/�B
0UB
/�B
/�B
0oB
1AB
1�B
2aB
3B
3�B
3�B
3�B
3MB
3�B
3�B
4B
4B
4nB
4�B
4nB
4�B
4�B
5B
5%B
5�B
5�B
6FB
6zB
6�B
6�B
6�B
6�B
7LB
88B
8RB
88B
8RB
8lB
8lB
8lB
8�B
8�B
8�B
8�B
8�B
9XB
9	B
9XB
8�B
9rB
9�B
:DB
:xB
:�B
:�B
:�B
:DB
9�B
:B
:DB
9�B
:*B
:�B
:�B
;�B
<6B
<�B
<�B
=B
="B
<�B
="B
=B
<�B
<�B
<�B
=<B
=�B
>BB
>�B
?�B
@iB
@iB
@�B
@iB
@ B
@B
@�B
@�B
@�B
@�B
A�B
A�B
BAB
B[B
BAB
B[B
B�B
B�B
CB
C�B
DB
DMB
D�B
C�B
D�B
DB
DB
D�B
D�B
FYB
F?B
F�B
F�B
F�B
GB
GB
G+B
GEB
GzB
H1B
H�B
H�B
H�B
I7B
IB
H�B
IB
I�B
I�B
I�B
J	B
JXB
KB
KDB
KxB
K�B
L0B
LdB
L~B
L~B
LdB
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N"B
NVB
NVB
NpB
NVB
NpB
N�B
N�B
O(B
O(B
O(B
OvB
O�B
P.B
P�B
Q B
Q B
Q�B
Q�B
R B
RTB
RTB
SB
SuB
S�B
S�B
T,B
T{B
T�B
U2B
UMB
UMB
U�B
VmB
V�B
V�B
W$B
W?B
W�B
W�B
W�B
W�B
XEB
X_B
X_B
XyB
X�B
YeB
YeB
Y�B
Y�B
Y�B
Y�B
Y�B
ZkB
Z�B
Z�B
[#B
[=B
[=B
[�B
[�B
\CB
\xB
\�B
\�B
]/B
]dB
]�B
]�B
^B
]�B
]�B
]�B
^5B
^jB
^�B
^�B
_VB
_pB
_pB
_VB
_�B
`\B
`�B
a�B
a�B
a�B
bNB
b�B
b�B
b�B
b�B
b�B
bhB
bhB
b�B
b�B
c�B
c�B
c�B
c�B
dB
d&B
d�B
d�B
eB
eFB
ezB
e�B
e�B
e�B
e�B
e�B
f2B
f�B
gB
gB
g8B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h$B
h�B
h�B
h�B
h�B
h�B
hsB
h�B
iB
i*B
i�B
jKB
j�B
jeB
jB
jKB
jeB
j�B
k�B
lB
l"B
m�B
m�B
m�B
m�B
mCB
m�B
m�B
nB
n�B
n�B
o B
o5B
oOB
oiB
o�B
o�B
o�B
o�B
o�B
pB
p!B
p�B
p�B
p�B
q'B
q'B
qAB
q�B
q�B
q�B
q�B
raB
r�B
sB
s3B
sMB
s�B
s�B
s�B
s�B
s�B
shB
s�B
t9B
tnB
u%B
u%B
u?B
utB
uZB
utB
u�B
u�B
u�B
vB
v+B
v+B
vFB
v�B
v�B
wB
wB
wLB
wfB
wfB
w�B
w�B
xB
xlB
x�B
x�B
x�B
x�B
y	B
y$B
y>B
y�B
y�B
y�B
zDB
z�B
|6B
|PB
|PB
|PB
|PB
|PB
|6B
|jB
|�B
}B
|�B
|�B
}B
}<B
}VB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~(B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~B
~B
~B
~B
}�B
}�B
}�B
}�B
}�B
~(B
~B
~�B
~�B
~�B
~�B
HB
cB
�B
�4B
��B
��B
��B
�B
�AB
�uB
��B
��B
�-B
�GB
�{B
�{B
��B
�B
�3B
�3B
�MB
�MB
�MB
��B
��B
��B
��B
��B
��B
�B
�9B
�S1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	*eB	*KB	*�B	*�B	*eB	*B	*B	)�B	*B	*B	*eB	)�B	)�B	*eB	+6B	+�B	+�B	+kB	+6B	*0B	(�B	)yB	./B	\]B	y�B	��B	x�B	{�B	��B	�B	�)B	�B	�8B
@�B
��B
�`B
�<B�B
��B
��B
�HB
��B
�TB!|B/5B'B'�B3hB>(BM�BR�BSuBH�BH�BMPBAB?�BHfBVBpUBj�Bm�B.B�B}�B�7Bx�B~wBy�Bj�Bs�BLdB�B[B
��B
�B
��B
ЗB
ǔB
��B
�B
y�B
I�B
B	�B	ݲB	��B	��B	��B	�_B	�uB	��B	q[B	Z�B	F�B	:�B	1vB	;B	�B	�B	PB��B�B��B�B�LB�-B��B�jB�B�B��B�B�5B�?BөB�2B��B�_B�dB�B�kB��B��B��B��B��B��B��B	�B	_B	B	!�B	&�B	,=B	A;B	�_B	~]B	��B	~�B	x�B	�B	�	B	��B	�TB	�<B	�6B	�gB	��B	��B	�B	�VB	�KB	�BB	��B	��B	��B	��B	��B	��B	��B	�$B	�:B	�>B	ªB	��B	�	B	��B	�4B	οB	�WB	�5B	��B	��B	�B	�B	�B	�B	�B	��B	��B	�|B	��B	�B	�,B	��B	ңB	��B	��B	یB	��B	޸B	�B	��B	��B	�B	�B	��B	�XB	�B	�]B	��B	�B	�TB	�B	�2B	�ZB	�B	��B	�LB	�B	��B	��B	�JB	��B	��B	��B	�BB	�B	��B	�]B	�wB	�.B	��B	��B
 iB	�}B	�BB	�B	�VB	��B	��B	��B	��B	��B	��B	��B	�nB	�ZB	��B	�8B	�B	�xB	��B	��B	��B	�B	��B	��B	�AB	�wB	�B	� B	��B	�B	��B	��B	��B	�LB	�FB	�`B	��B	�$B	��B	��B	��B	��B	�B	�B	�*B	��B	��B	�*B	�B	��B	��B	�B	�yB	�B	�B	�;B	�B	�B	�B	�|B	�UB	��B	��B	��B	��B	�B	�B	�kB	�B	�[B	�'B	��B	�)B	��B	�yB	�B	�B	�B	�@B	��B	�@B	�,B	�B	�,B	�,B	�8B	�mB	��B	�B	�@B	�B	�&B	�B	�B	�B	�
B	�B	�*B	�B	�B	�B	��B	��B	�GB	�B	�5B	�"B	�qB	�B	�]B	��B	�cB	�/B	�iB	�B	��B	��B	��B	�B	�;B	�B	��B	��B	�B	� B	�B	�OB	�B	�B	�-B	�B	�-B	�B	��B	��B	��B	�B	�FB	�FB	��B	�B	��B	�2B	��B	��B	�$B	��B	��B	�6B
 �B
B
�B
MB
B
�B
GB
aB
�B
�B
�B
B
gB
MB
�B
fB
YB
�B
%B
tB
�B
�B
�B
�B
�B
zB
EB
zB
zB
�B
�B
�B
	B
	RB
	�B

rB

�B

�B
B
�B
�B
PB
jB
�B
�B
�B
�B
HB
�B
B
�B
�B
�B
:B
:B
TB
TB
oB
&B
�B
�B
�B
FB
aB
�B
MB
�B
B
9B
mB
�B
$B
$B
$B
YB
sB
�B
�B
EB
�B
B
eB
�B
KB
B
�B
7B
�B
�B
#B
=B
=B
qB
�B
xB
�B
�B
�B
B
B
B
�B
�B
�B
�B
pB
VB
pB
�B
�B
 �B
 �B
 �B
 �B
 �B
!bB
!HB
!�B
!�B
"hB
"hB
"�B
"�B
#B
#nB
#�B
$&B
%�B
%�B
%�B
&LB
'�B
(�B
(sB
(�B
)*B
)yB
)*B
)B
)B
)�B
)*B
)DB
)yB
)�B
)�B
*KB
*eB
*�B
*�B
*�B
+6B
+�B
,"B
,"B
,=B
,�B
-]B
-�B
.IB
/5B
/�B
/�B
/�B
0;B
0�B
0�B
0oB
0!B
0B
0�B
/�B
0B
0�B
1�B
2GB
2�B
33B
3�B
3�B
3�B
3�B
3�B
3�B
4B
49B
4�B
4�B
4�B
4�B
5%B
5?B
5?B
5�B
5�B
6`B
6�B
6�B
6�B
6�B
6�B
7�B
88B
8lB
8RB
8lB
8lB
8�B
8�B
9	B
8�B
8�B
8�B
8�B
9rB
9$B
9rB
9$B
9rB
:B
:^B
:�B
:�B
:�B
:�B
:^B
9�B
:DB
:^B
9�B
:^B
:�B
;0B
<B
<jB
<�B
="B
=qB
=VB
=B
=VB
="B
<�B
<�B
<�B
=qB
=�B
>wB
?B
@ B
@�B
@�B
@�B
@�B
@B
@4B
@�B
@�B
AB
A B
A�B
B'B
BuB
B[B
B[B
BuB
B�B
CB
CB
C�B
D3B
D�B
D�B
D�B
D�B
DB
DMB
E9B
E9B
F�B
FYB
F�B
F�B
F�B
GB
GB
GEB
GzB
G�B
HfB
H�B
H�B
I7B
IRB
IB
H�B
I7B
I�B
I�B
J#B
J=B
JrB
K)B
K^B
K�B
K�B
L0B
L~B
L�B
L�B
L~B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N"B
NpB
NpB
NpB
NpB
N�B
N�B
OB
OBB
OBB
O\B
O�B
O�B
P}B
P�B
Q4B
Q4B
Q�B
Q�B
R:B
R�B
R�B
S@B
S�B
S�B
S�B
TaB
T{B
T�B
U2B
UMB
UgB
U�B
V�B
V�B
V�B
W?B
W?B
W�B
W�B
XB
W�B
X_B
XyB
XyB
X�B
X�B
YeB
YeB
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
[#B
[WB
[WB
[�B
[�B
\]B
\xB
\�B
\�B
]IB
]�B
]�B
]�B
^B
]�B
]�B
^B
^OB
^�B
^�B
_B
_pB
_�B
_�B
_pB
`B
`\B
`�B
a�B
a�B
a�B
bhB
b�B
cB
b�B
b�B
b�B
bhB
b�B
b�B
b�B
c�B
c�B
c�B
c�B
c�B
d@B
d�B
d�B
eB
e`B
e�B
e�B
e�B
e�B
e�B
e�B
f2B
f�B
gB
gB
gRB
g�B
g�B
g�B
g�B
h
B
h
B
g�B
h$B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
iB
i_B
i�B
jKB
j�B
j�B
j�B
jeB
jB
j�B
k�B
l"B
l=B
m�B
m�B
m�B
m�B
m]B
m�B
m�B
n/B
n�B
n�B
oB
oOB
oOB
oiB
o�B
o�B
o�B
o�B
o�B
pB
p;B
p�B
p�B
p�B
qAB
qAB
q[B
q�B
q�B
q�B
rB
r|B
r�B
s3B
s3B
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t9B
tTB
u%B
u%B
uZB
u�B
utB
u�B
u�B
u�B
u�B
v+B
v+B
vFB
v`B
v�B
v�B
wB
wB
wLB
wfB
wfB
w�B
w�B
xB
xlB
x�B
x�B
x�B
x�B
y	B
y	B
y$B
y�B
y�B
y�B
z*B
z�B
|PB
|jB
|PB
|PB
|PB
|jB
|6B
|jB
}B
}"B
|�B
}B
}"B
}<B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~BB
}�B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~(B
~(B
~(B
~(B
}�B
}�B
}�B
~B
}�B
~(B
~(B
~�B
~�B
~�B
~�B
HB
}B
�B
�B
��B
��B
��B
�B
�'B
��B
��B
�B
�GB
�aB
��B
��B
��B
�B
�3B
�B
�MB
�MB
�gB
��B
��B
��B
��B
��B
�B
�B
�9B
�S3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.07(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201908220051272019082200512720190822005127202207271132192022072711321920220727113219202207271534562022072715345620220727153456  JA  ARFMdecpA30a                                                                20190811093652  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190811093723  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190811093724  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190811093725  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190811093725  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190811093725  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190811093725                      G�O�G�O�G�O�                JA  ARUP                                                                        20190811095555                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190812000000  CF  PSAL_ADJUSTED_QC@Q�@Q�G�O�                JM  ARCAJMQC2.0                                                                 20190821155127  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190821155127  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023219  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063456  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081508                      G�O�G�O�G�O�                