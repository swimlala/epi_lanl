CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-04-22T09:41:56Z creation;2023-04-22T09:41:57Z conversion to V3.1      
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
_FillValue                 �  I0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ph   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Έ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �h   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �h   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �h   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230422094156  20230422095722  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @�%��c��1   @�%���k@;�z�G��c��S���1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���C   C  C�C  C  C	�fC  C  C  C�fC  C  C  C  C�fC  C�fC"�C$�C&  C(  C)�fC,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ�C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cu�fCx  Cz  C{�fC~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C��C�  C��3C��3C�  C��C�  C��3C��3C�  C�  C��C�  C�  C��3C��3C��3C��3C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C��C��C�  C�  C��3C�  C��3C��3C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D0��D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8�fD9  D9� D:fD:� D:��D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJfDJ� DK  DK� DK��DLy�DM  DM� DN  DN� DO  DO� DPfDP� DP��DQ� DR  DR�fDS  DS� DT  DT� DU  DUy�DU��DVy�DV��DW� DX  DX� DY  DY� DY��DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg�fDhfDh�fDifDi�fDj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy�fDzfDz� D{  D{y�D{��D|� D}  D}� D~  D~� D  D� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D��3D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D D�� D�  D�<�DÀ D�� D�  D�C3DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ Dɼ�D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�<�D΀ D�� D�  D�@ Dπ D��3D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�<�DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D��3D�3D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ Dڼ�D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ Dݼ�D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�<�D�� D�� D�  D�@ D� D�� D���D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D�� D�� D�  D�@ D� D�� D�  D�@ D� D��D���D�@ D� D�� D�  D�@ D� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�#311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@z�G@�=q@�=qA�A>�RA]�A}�A��\A��\A��\A��\AΏ\Aޏ\A�\A��\BG�BG�BG�B�HB'G�B/G�B7G�B?G�BG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B��
Bã�Bǣ�B��
Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��
B��B��B���B�p�B���C��C�C��C��C	�RC��C��C��C�RC��C��C��C��C�RC��C�RC!�C#�C%��C'��C)�RC+�C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS�CU��CW��CY�C[��C]��C_�RCa��Cc��Ce��Cg��Ci��Ck��Cm�Co��Cq��Cs��Cu�RCw��Cy��C{�RC}��C�RC���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C��)C���C���C���C��)C��)C���C���C���C���C���C��)C��)C��)C��)C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C��)C��)C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C��)C���C���C���C���C���C���C���C���C���C��)C���C��)C��)C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dz�D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8z�D8�{D9t{D9��D:t{D:�D;nD;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBz�DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIz�DI��DJt{DJ�{DKt{DK�DLnDL�{DMt{DM�{DNt{DN�{DOt{DO��DPt{DP�DQt{DQ�{DRz�DR�{DSt{DS�{DTt{DT�{DUnDU�DVnDV�DWt{DW�{DXt{DX�{DYt{DY�DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgz�Dg��Dhz�Dh��Diz�Di�{Djt{Dj��Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyz�Dy��Dzt{Dz�{D{nD{�D|t{D|�{D}t{D}�{D~t{D~�{Dt{D�{D�7
D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�7
D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��pD��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��pD�=pD�z=D��=D��=D�:=D�z=D��pD��=D�:=D�z=D��
D��
D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�}pD��=D��=D�:=D�z=D��pD��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�=pD�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��
D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��pD��pD�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��pD�=pD�}pD��pD��pD�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�=pD�z=Dº=D��=D�7
D�z=Dú=D��=D�=pD�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�z=Dƺ=D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɷ
D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�7
D�z=Dκ=D��=D�:=D�z=DϽpD��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�7
D�z=DԺ=D��=D�:=D�z=Dպ=D��=D�:=D�z=Dֺ=D��=D�:=D�z=D׽pD��pD�:=D�z=Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dڷ
D��=D�:=D�z=Dۺ=D��=D�:=D�z=Dܺ=D��=D�:=D�z=Dݷ
D��=D�:=D�z=D޺=D��=D�:=D�z=Dߺ=D��=D�7
D�z=D�=D��=D�:=D�z=D�=D��
D�7
D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��pD�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�7
D�w
D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��
D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�7
D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�
D��
D�:=D�z=D�=D��=D�:=D�z=D��pD��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�p11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A�w2A��$A�9�A���A���A�oiA�!-A���A�I�A��TA��A�ĜA���A�A��HA�I�A�%A���A��RA�u%A�VmA�&�A��A��A��wA��CA��1A�|A�n�A�YA�.A��A�� A��)A�F?A��}A�&�A��A��
A��A���A��UA�h
A�GzA�<A��A��vA��A��zA��^A��~A��YA��LA��eA��+A�u�A�d�A�Y�A�M�A�9$A�2aA� �A���A���A���A��:A���A�e,A�A A�!A�
rA�� A�2�A���A�2-A���A��JA�P}A���A�&A��oA�"�A�7LA��9A�r�A���A�cTA���A�jKA�ĜA���A�k�A�|PA��RA�33A��A���A��[A��A�  A�SA�� A���A��A��A�uA~�FA}��A|*�Au��As�bAr�^Ara�Aq�ApOAnݘAn�Ak�SAi�Ag��AgS&Af�Ae�DAe�Ad�Acg8Ab�9Ab6AaU2A`r�A_یA^�.A\��A[�sAZ�EAYAXSAWN<AT�AR�bAO�AO�ANiDAL�oAJ5�AI1AH�&AH�9AH��AH�$AH�fAHJ�AG]dAF+AD�2AD�IAD{AC�AA^5A?��A?.IA>�A=�jA=Z�A<��A<�sA<H�A;��A;#:A:.�A9m�A95?A8�&A8*0A78A6.IA5�A4��A4p�A3��A3�A0�A/��A/=A.��A.8�A-��A,�A,4A+�A*�DA)�A)�LA)[�A(�A(5�A'A%ںA$d�A"��A!��A!�A ��A 1�A�;A��Ae�A�Ax�A�A��A|�A~�AU2Ai�A-wA�A�/A�Az�A�EAzA�oAzA�dA\�AN�A4�ArGA<6AqA��A�-A'RA
�1A
8A	�}A	S�A	qA��A��A��A �A6�AW?A�AJ�A��A��A��Ak�A:*A��A�AY�A ��A \)@�w2@�)_@��H@��@��x@�S@�͟@�&�@�YK@�>B@�\�@��A@��,@�-@�}@�h@��@߳�@��o@�U�@܊r@�u�@ڨ�@�e@٦�@ؖ�@ס�@�x@��@�-�@Ӣ�@��	@ѯ�@�5?@��T@��v@�Xy@��z@̨�@˫�@�Dg@��W@�<6@Ȁ�@��+@ǫ�@�j@�6z@�/@��,@å@�A�@�X�@���@�Z�@��4@��W@��B@�c�@��}@�6z@��@�C�@�z@��Y@�@��|@�V@�<�@�6�@��@��@�8�@�c�@�o�@�˒@��]@���@��@�ѷ@���@�u%@���@�j@��}@���@�m]@���@�M@�=�@�S@��2@���@��@���@���@�O@��@�w2@��L@�Ov@���@���@��@���@��H@���@�W?@��@�!@��4@���@�@���@�]�@���@�e�@�O@��j@�B�@��O@�p;@�Q@�b@��D@��@��@���@�a�@�@��Y@�c @�<�@���@���@�4�@���@�	@��>@���@�T�@�@�c @�(�@���@���@�$t@���@��@��+@��o@�z�@�ff@�&�@��A@��$@�ߤ@�z�@�W�@�0U@��D@���@��S@��{@�p�@�^�@�O@��@��O@��@�ƨ@�u�@�a@�L�@�:�@�)_@�:�@�E9@�4�@��P@��U@���@�S&@�>�@�5�@��@���@��3@�o�@��`@�($@���@��g@�Y�@�*0@��@�ѷ@�xl@���@���@��b@�~(@�Ov@�@�s@�S�@�#�@���@��)@��@�8�@�@�k@y�@P�@$t@~�@~��@~�@}p�@}�@|��@|�I@|�$@|�U@|tT@{�$@{C�@{�@{_p@}�@}B�@|��@|�@{e�@{�@zz@z.�@y�.@y�@x`�@xb@w�&@w��@w_p@w�@v�@v~�@u�@u�@u�@u�'@u��@v$�@u�Z@u�'@u�@tɆ@tw�@tV�@tA�@t�@s�K@r��@r�@qrG@o�&@o�@n6�@m@m��@mL�@ka@j��@j�r@jff@jQ@j?@j$�@j �@i��@i|@i/@h�f@h�@gS�@f��@f��@f�b@fz@f-@e��@d��@d�.@dXy@dK^@dM@d�e@d�.@d��@d��@dZ@c��@cl�@c+@a�@`��@`?�@`1'@_b�@^�h@^	@]��@]zx@]Y�@]`B@]rG@]}�@]f�@]�@\��@\U2@[�&@[t�@[@O@[�@Z��@Zh
@Z�@Y��@Y4@XɆ@X�e@Xz�@X2�@W��@V��@VL0@V{@U�7@U�@T�U@T]d@S��@S��@S��@S'�@R��@R��@Rq�@R;�@Q�=@P�|@Pe�@P�@Oݘ@Ox@OU�@O$t@N�@N�<@N��@NJ�@N$�@M��@M��@Mf�@M<6@M�@L�p@LbN@L-�@K�*@K
=@J�,@J��@I�D@IIR@Hی@Hoi@HN�@H�@G�@G�4@G�@F�X@F��@F\�@F@E��@ES&@E@@D�E@D�z@DQ�@C�W@C��@C�	@C�@B�m@Bd�@B+k@A�@A�N@A�3@A�3@A��@A�3@A��@A@A�@A}�@A^�@A2a@@ѷ@@�@?��@?��@?o�@?+@>�!@>=q@=�D@=��@=�n@=�S@=�7@=��@=G�@<��@<�$@<V�@;��@;�{@;e�@;Mj@;>�@:�y@:�'@:��@:V@:-@9�@9Dg@8�/@8��@8`�@8�@8�@8�@7�@7˒@7�[@7��@7��@7'�@6��@6�x@6�A@6C�@5�@5�~@5S&@50�@5&�@5�@5�@5;@4�@4��@4U2@4'R@3��@3�6@3�$@3C�@3@2�@2�@2�c@2�@2�h@2��@2z@1��@1�@1��@1��@1j@1/@0�@0�@0-�@0(�@0'R@0�@/��@/��@/qv@/F�@/!-@.�@.��@.�b@.�\@.}V@.a|@-^�@,�@,��@,>B@,�@,G@+��@+خ@+�}@+�a@+�q@+��@+��@+�@+�$@+��@+{J@+!-@+�@*��@*�@*�y@*��@)�.@)�@(�?@(��@(��@(��@(�Y@(N�@(D�@(4n@(�@'�V@'dZ@'W?@'C�@',�@&�"@&ȴ@&��@&xl@%��@%��@%�=@%Dg@%0�@$�f@$�v@$��@$V�@$@#�
@#�k@#��@#U�@#8@"�@"��@"��@"�r@"n�@"W�@"=q@"#:@"#:@!��@!w2@!&�@ ی@ ѷ@ ��@ u�@ _@ C-@  �@��@��@�V@�V@�P@�@�y@��@�@�A@\�@&�@ �@�j@�^@�C@o @%F@�@�@��@�Y@oi@H@2�@~@G@��@_p@"�@
=@�8@�@��@�H@��@�A@l�@W�@($@�o@�X@��@u�@\�@(�@�@�)@�I@j@b@�m@خ@x@6z@�]@xl@h
@Z�@+k@�@��@�#@�9@ϫ@��@��@�@�~@rG@N<@��@7�@�W@�@y�@�@��@��@��@��@v�@_�@V@M�@1�@�@��@p�@0�@-w@*0@�@�.@@��@�{@W?@;d@��@�@��@3�@x�@:�@5�@�@�)@��@�@h�@M@Ft@>B@/�@	�@��@�K@a@_p@a@>�@�@
�,@
v�@
;�@
($@
4@	��@	��@	\�@	*0@��@�$@��@[�@S�@U2@V�@M@'R@b@�g@��@�@y�@t�@.I@�\@h
@=q@$�@�@�@��@N<@(�@�@ی@�.@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A�w2A��$A�9�A���A���A�oiA�!-A���A�I�A��TA��A�ĜA���A�A��HA�I�A�%A���A��RA�u%A�VmA�&�A��A��A��wA��CA��1A�|A�n�A�YA�.A��A�� A��)A�F?A��}A�&�A��A��
A��A���A��UA�h
A�GzA�<A��A��vA��A��zA��^A��~A��YA��LA��eA��+A�u�A�d�A�Y�A�M�A�9$A�2aA� �A���A���A���A��:A���A�e,A�A A�!A�
rA�� A�2�A���A�2-A���A��JA�P}A���A�&A��oA�"�A�7LA��9A�r�A���A�cTA���A�jKA�ĜA���A�k�A�|PA��RA�33A��A���A��[A��A�  A�SA�� A���A��A��A�uA~�FA}��A|*�Au��As�bAr�^Ara�Aq�ApOAnݘAn�Ak�SAi�Ag��AgS&Af�Ae�DAe�Ad�Acg8Ab�9Ab6AaU2A`r�A_یA^�.A\��A[�sAZ�EAYAXSAWN<AT�AR�bAO�AO�ANiDAL�oAJ5�AI1AH�&AH�9AH��AH�$AH�fAHJ�AG]dAF+AD�2AD�IAD{AC�AA^5A?��A?.IA>�A=�jA=Z�A<��A<�sA<H�A;��A;#:A:.�A9m�A95?A8�&A8*0A78A6.IA5�A4��A4p�A3��A3�A0�A/��A/=A.��A.8�A-��A,�A,4A+�A*�DA)�A)�LA)[�A(�A(5�A'A%ںA$d�A"��A!��A!�A ��A 1�A�;A��Ae�A�Ax�A�A��A|�A~�AU2Ai�A-wA�A�/A�Az�A�EAzA�oAzA�dA\�AN�A4�ArGA<6AqA��A�-A'RA
�1A
8A	�}A	S�A	qA��A��A��A �A6�AW?A�AJ�A��A��A��Ak�A:*A��A�AY�A ��A \)@�w2@�)_@��H@��@��x@�S@�͟@�&�@�YK@�>B@�\�@��A@��,@�-@�}@�h@��@߳�@��o@�U�@܊r@�u�@ڨ�@�e@٦�@ؖ�@ס�@�x@��@�-�@Ӣ�@��	@ѯ�@�5?@��T@��v@�Xy@��z@̨�@˫�@�Dg@��W@�<6@Ȁ�@��+@ǫ�@�j@�6z@�/@��,@å@�A�@�X�@���@�Z�@��4@��W@��B@�c�@��}@�6z@��@�C�@�z@��Y@�@��|@�V@�<�@�6�@��@��@�8�@�c�@�o�@�˒@��]@���@��@�ѷ@���@�u%@���@�j@��}@���@�m]@���@�M@�=�@�S@��2@���@��@���@���@�O@��@�w2@��L@�Ov@���@���@��@���@��H@���@�W?@��@�!@��4@���@�@���@�]�@���@�e�@�O@��j@�B�@��O@�p;@�Q@�b@��D@��@��@���@�a�@�@��Y@�c @�<�@���@���@�4�@���@�	@��>@���@�T�@�@�c @�(�@���@���@�$t@���@��@��+@��o@�z�@�ff@�&�@��A@��$@�ߤ@�z�@�W�@�0U@��D@���@��S@��{@�p�@�^�@�O@��@��O@��@�ƨ@�u�@�a@�L�@�:�@�)_@�:�@�E9@�4�@��P@��U@���@�S&@�>�@�5�@��@���@��3@�o�@��`@�($@���@��g@�Y�@�*0@��@�ѷ@�xl@���@���@��b@�~(@�Ov@�@�s@�S�@�#�@���@��)@��@�8�@�@�k@y�@P�@$t@~�@~��@~�@}p�@}�@|��@|�I@|�$@|�U@|tT@{�$@{C�@{�@{_p@}�@}B�@|��@|�@{e�@{�@zz@z.�@y�.@y�@x`�@xb@w�&@w��@w_p@w�@v�@v~�@u�@u�@u�@u�'@u��@v$�@u�Z@u�'@u�@tɆ@tw�@tV�@tA�@t�@s�K@r��@r�@qrG@o�&@o�@n6�@m@m��@mL�@ka@j��@j�r@jff@jQ@j?@j$�@j �@i��@i|@i/@h�f@h�@gS�@f��@f��@f�b@fz@f-@e��@d��@d�.@dXy@dK^@dM@d�e@d�.@d��@d��@dZ@c��@cl�@c+@a�@`��@`?�@`1'@_b�@^�h@^	@]��@]zx@]Y�@]`B@]rG@]}�@]f�@]�@\��@\U2@[�&@[t�@[@O@[�@Z��@Zh
@Z�@Y��@Y4@XɆ@X�e@Xz�@X2�@W��@V��@VL0@V{@U�7@U�@T�U@T]d@S��@S��@S��@S'�@R��@R��@Rq�@R;�@Q�=@P�|@Pe�@P�@Oݘ@Ox@OU�@O$t@N�@N�<@N��@NJ�@N$�@M��@M��@Mf�@M<6@M�@L�p@LbN@L-�@K�*@K
=@J�,@J��@I�D@IIR@Hی@Hoi@HN�@H�@G�@G�4@G�@F�X@F��@F\�@F@E��@ES&@E@@D�E@D�z@DQ�@C�W@C��@C�	@C�@B�m@Bd�@B+k@A�@A�N@A�3@A�3@A��@A�3@A��@A@A�@A}�@A^�@A2a@@ѷ@@�@?��@?��@?o�@?+@>�!@>=q@=�D@=��@=�n@=�S@=�7@=��@=G�@<��@<�$@<V�@;��@;�{@;e�@;Mj@;>�@:�y@:�'@:��@:V@:-@9�@9Dg@8�/@8��@8`�@8�@8�@8�@7�@7˒@7�[@7��@7��@7'�@6��@6�x@6�A@6C�@5�@5�~@5S&@50�@5&�@5�@5�@5;@4�@4��@4U2@4'R@3��@3�6@3�$@3C�@3@2�@2�@2�c@2�@2�h@2��@2z@1��@1�@1��@1��@1j@1/@0�@0�@0-�@0(�@0'R@0�@/��@/��@/qv@/F�@/!-@.�@.��@.�b@.�\@.}V@.a|@-^�@,�@,��@,>B@,�@,G@+��@+خ@+�}@+�a@+�q@+��@+��@+�@+�$@+��@+{J@+!-@+�@*��@*�@*�y@*��@)�.@)�@(�?@(��@(��@(��@(�Y@(N�@(D�@(4n@(�@'�V@'dZ@'W?@'C�@',�@&�"@&ȴ@&��@&xl@%��@%��@%�=@%Dg@%0�@$�f@$�v@$��@$V�@$@#�
@#�k@#��@#U�@#8@"�@"��@"��@"�r@"n�@"W�@"=q@"#:@"#:@!��@!w2@!&�@ ی@ ѷ@ ��@ u�@ _@ C-@  �@��@��@�V@�V@�P@�@�y@��@�@�A@\�@&�@ �@�j@�^@�C@o @%F@�@�@��@�Y@oi@H@2�@~@G@��@_p@"�@
=@�8@�@��@�H@��@�A@l�@W�@($@�o@�X@��@u�@\�@(�@�@�)@�I@j@b@�m@خ@x@6z@�]@xl@h
@Z�@+k@�@��@�#@�9@ϫ@��@��@�@�~@rG@N<@��@7�@�W@�@y�@�@��@��@��@��@v�@_�@V@M�@1�@�@��@p�@0�@-w@*0@�@�.@@��@�{@W?@;d@��@�@��@3�@x�@:�@5�@�@�)@��@�@h�@M@Ft@>B@/�@	�@��@�K@a@_p@a@>�@�@
�,@
v�@
;�@
($@
4@	��@	��@	\�@	*0@��@�$@��@[�@S�@U2@V�@M@'R@b@�g@��@�@y�@t�@.I@�\@h
@=q@$�@�@�@��@N<@(�@�@ی@�.@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Bg�Bf�Be�Bd�Bb�BcTBfLBg�Bm�BuBu�Bw�ByrB|B~wB��B�zB�B�BB��B��B�NB�NB� B�oB�$B�yB�B�B�B��B��B�9B��B�[B�1B��B��B�B�vB�BB�(B�B�B�SB�TB��B��B�4B� B��B��B��B�<B��B�7B��B�B�,B��B�uB� B��B��B��B�~B�rB�_B��B�{B�B{JBw�BpUB_�BW$BDgB<6B/�B$tB�B�B��B�QB��Ba�BEBpB�B��B�uBÖB�*B��B��Bz�Bg�BM�BH�B?HB,B#:B�B"B�BB �B
��B
�B
�QB
��B
��B
��B
�B
�&B
�TB
�B
��B
�HB
�B
|�B
t�B
raB
p�B
nB
kQB
gB
d&B
aB
^B
[WB
W$B
S�B
P�B
H�B
DMB
AB
;B
5%B
.}B
�B
{B	�B	�B	�B	��B	��B	�jB	͟B	�pB	��B	�B	��B	�@B	��B	��B	�rB	ƎB	�uB	��B	�lB	��B	�QB	�sB	�XB	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�(B	��B	��B	�xB	�_B	��B	}B	{0B	y	B	v`B	uZB	q�B	m]B	i�B	gB	dZB	bhB	`�B	_!B	[�B	WsB	R B	K�B	C�B	>�B	;�B	9�B	7�B	5�B	/�B	($B	#nB	 �B	5B	B	\B	
#B	+B	3B	AB	�B	UB	 �B	 4B��B�B��B��B��B��B�%B�TB�B�B�oB�IB�=B��B�*B�>B�8B��B��B�`B�`B�ZB�B��B�-B�!B�VBݲB�/B��B�	B�B�B�EB�B�9B��B�FBҽB҉BбB��B�(B�B͟B�xB�6BΥB��B��B�6B��B�=B�B��BňBƎB�RB�B�_B��B�+BɺB˒B�"B��B��B�B�:B�B��B�yB�B��B��BۦB��BܒB�jB�jB�B��B��B�B�B�TB�tB�B�RB�>B�B�B�0B�qB��B��B�B��B�B�iB�B��B��B��B��B�B��B�PB�jB�wB	B	{B	�B		lB		B	�B	�B	�B	�B	�B	�B	#B	B	IB	�B	 'B	�B	 vB	 �B	!-B	!�B	!�B	#�B	%zB	&�B	+6B	0!B	1B	5tB	=qB	=�B	>B	>]B	?�B	@OB	@�B	D3B	F�B	K�B	N�B	TaB	V9B	Y�B	[qB	]B	^B	bB	e�B	g�B	h
B	i�B	i�B	jB	jB	k6B	mB	n�B	p�B	poB	qB	r�B	u%B	xB	{�B	��B	�B	�gB	�B	�KB	�~B	��B	�pB	�bB	��B	�B	�B	��B	�B	��B	�B	��B	�HB	��B	��B	�eB	�B	�WB	��B	��B	��B	��B	��B	�hB	�TB	��B	��B	��B	��B	�OB	��B	��B	��B	�B	ȀB	��B	̘B	��B	бB	��B	�gB	՛B	յB	�B	�eB	��B	�;B	�HB	��B	�`B	�B	�B	��B	�eB	�kB	�B	��B	�nB	�?B	��B	��B	�B	�>B	��B	��B	�VB	��B
 �B
�B
YB
�B
zB
�B
1B
�B
	B
�B
BB
 B
�B
yB
B
�B
�B
B
�B
!bB
#�B
+�B
.�B
1�B
3�B
6B
6�B
8�B
9XB
9rB
=qB
?�B
A;B
A�B
B�B
D3B
ESB
FB
GEB
JXB
KxB
L~B
M�B
N�B
S@B
U�B
U�B
U�B
VB
VSB
WsB
XyB
XEB
W�B
X�B
]IB
]/B
\)B
\CB
]dB
]~B
]�B
^B
bhB
cnB
c�B
d�B
d�B
e`B
e�B
gB
g�B
iyB
i�B
kB
m�B
o B
pB
p�B
q'B
qvB
r-B
shB
s�B
t�B
u�B
vFB
wLB
y	B
z�B
}VB
�B
��B
�GB
�{B
�3B
�B
�9B
�mB
�9B
��B
�YB
��B
�#B
��B
�B
�TB
��B
�&B
��B
�B
�
B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�!B
��B
�B
�HB
��B
�&B
�ZB
��B
��B
�>B
�DB
�eB
��B
�kB
�qB
��B
��B
��B
��B
��B
�vB
��B
�3B
��B
��B
��B
�tB
��B
�fB
��B
�RB
��B
��B
�*B
��B
��B
�0B
��B
��B
�B
�]B
��B
��B
��B
� B
�B
��B
ªB
ªB
�B
��B
ĜB
��B
�tB
�EB
�_B
ȀB
�RB
�=B
��B
��B
�0B
̘B
͹B
�pB
ΊB
ϑB
�.B
бB
�4B
� B
�TB
�oB
�oB
҉B
�oB
�oB
�oB
ңB
�&B
�[B
өB
ԕB
�mB
��B
�?B
�?B
׍B
�EB
��B
�1B
�B
ٴB
ٴB
��B
ٴB
�7B
چB
�	B
یB
�xB
ܬB
��B
��B
��B
ݘB
��B
�B
ޞB
ޞB
�!B
��B
��B
��B
�B
��B
�4B
�B
�NB
�B
��B
��B
�B
�B
�ZB
�tB
�B
��B
��B
�LB
�B
��B
��B
��B
�B
�B
�RB
�B
�>B
�sB
��B
�B
�_B
�B
�B
�B
�B
�B
�B
�B
�QB
�6B
�qB
��B
��B
��B
�B
�wB
��B
�B
�5B
� B
� B
�OB
��B
�B
�;B
�oB
��B
�'B
�vB
�[B
�vB
�B
�B
�B
�TB
��B
�ZB
��B
��B
��B
��B
�B
�+B
�`B
�`B
�zB
�zB
�zB
��B
��B
�LB
��B
��B
��B
��B
�B
��B
��B
��B
�B
�0B
�0B
�0B
��B
��B
��B
�B
��B
�"B
�VB
�qB
�qB
��B
�B
�B
�wB
�cB
�}B
��B OB 4B �B �B B�BBuB�B�BB-B�B�B�B3B�B�B�B�B�BB�BYB�B�BB_BzB�B�B1B�B�B�B�B	7B	�B	�B	�B	�B
#B
rB
�B
�B
�B
�B^B�B�B0BdB�B�BBB6BPB�B<B�B�B�BB�BBvB�B�B�B.BbB�B�B B BNB�B�BBTB�B�BB�B,B�BgBMBMB�B�B�B�B�B�B�B�BBBSBmB�B�B�B_ByBKB�B�B�BBB7BkBQB�B�B=B�B�B�B�B�B�B�BBjB�B�B;BVB�B B!HB!|B!bB!�B"B"�B"�B"�B"�B#B#B#:B#�B$B$&B$�B$�B$�B%,B%B%,B%�B&2B&2B&fB&�B'8B'�B'�B(�B)*B)�B)�B*0B*KB*KB*eB*�B*�B+QB+kB+�B+�B+�B,"B-B-)B-wB-�B-�B-�B.�B.�B/ B/ B/�B/�B/�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444Bg�Bf�Be�Bd�Bb�BcTBfLBg�Bm�BuBu�Bw�ByrB|B~wB��B�zB�B�BB��B��B�NB�NB� B�oB�$B�yB�B�B�B��B��B�9B��B�[B�1B��B��B�B�vB�BB�(B�B�B�SB�TB��B��B�4B� B��B��B��B�<B��B�7B��B�B�,B��B�uB� B��B��B��B�~B�rB�_B��B�{B�B{JBw�BpUB_�BW$BDgB<6B/�B$tB�B�B��B�QB��Ba�BEBpB�B��B�uBÖB�*B��B��Bz�Bg�BM�BH�B?HB,B#:B�B"B�BB �B
��B
�B
�QB
��B
��B
��B
�B
�&B
�TB
�B
��B
�HB
�B
|�B
t�B
raB
p�B
nB
kQB
gB
d&B
aB
^B
[WB
W$B
S�B
P�B
H�B
DMB
AB
;B
5%B
.}B
�B
{B	�B	�B	�B	��B	��B	�jB	͟B	�pB	��B	�B	��B	�@B	��B	��B	�rB	ƎB	�uB	��B	�lB	��B	�QB	�sB	�XB	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�(B	��B	��B	�xB	�_B	��B	}B	{0B	y	B	v`B	uZB	q�B	m]B	i�B	gB	dZB	bhB	`�B	_!B	[�B	WsB	R B	K�B	C�B	>�B	;�B	9�B	7�B	5�B	/�B	($B	#nB	 �B	5B	B	\B	
#B	+B	3B	AB	�B	UB	 �B	 4B��B�B��B��B��B��B�%B�TB�B�B�oB�IB�=B��B�*B�>B�8B��B��B�`B�`B�ZB�B��B�-B�!B�VBݲB�/B��B�	B�B�B�EB�B�9B��B�FBҽB҉BбB��B�(B�B͟B�xB�6BΥB��B��B�6B��B�=B�B��BňBƎB�RB�B�_B��B�+BɺB˒B�"B��B��B�B�:B�B��B�yB�B��B��BۦB��BܒB�jB�jB�B��B��B�B�B�TB�tB�B�RB�>B�B�B�0B�qB��B��B�B��B�B�iB�B��B��B��B��B�B��B�PB�jB�wB	B	{B	�B		lB		B	�B	�B	�B	�B	�B	�B	#B	B	IB	�B	 'B	�B	 vB	 �B	!-B	!�B	!�B	#�B	%zB	&�B	+6B	0!B	1B	5tB	=qB	=�B	>B	>]B	?�B	@OB	@�B	D3B	F�B	K�B	N�B	TaB	V9B	Y�B	[qB	]B	^B	bB	e�B	g�B	h
B	i�B	i�B	jB	jB	k6B	mB	n�B	p�B	poB	qB	r�B	u%B	xB	{�B	��B	�B	�gB	�B	�KB	�~B	��B	�pB	�bB	��B	�B	�B	��B	�B	��B	�B	��B	�HB	��B	��B	�eB	�B	�WB	��B	��B	��B	��B	��B	�hB	�TB	��B	��B	��B	��B	�OB	��B	��B	��B	�B	ȀB	��B	̘B	��B	бB	��B	�gB	՛B	յB	�B	�eB	��B	�;B	�HB	��B	�`B	�B	�B	��B	�eB	�kB	�B	��B	�nB	�?B	��B	��B	�B	�>B	��B	��B	�VB	��B
 �B
�B
YB
�B
zB
�B
1B
�B
	B
�B
BB
 B
�B
yB
B
�B
�B
B
�B
!bB
#�B
+�B
.�B
1�B
3�B
6B
6�B
8�B
9XB
9rB
=qB
?�B
A;B
A�B
B�B
D3B
ESB
FB
GEB
JXB
KxB
L~B
M�B
N�B
S@B
U�B
U�B
U�B
VB
VSB
WsB
XyB
XEB
W�B
X�B
]IB
]/B
\)B
\CB
]dB
]~B
]�B
^B
bhB
cnB
c�B
d�B
d�B
e`B
e�B
gB
g�B
iyB
i�B
kB
m�B
o B
pB
p�B
q'B
qvB
r-B
shB
s�B
t�B
u�B
vFB
wLB
y	B
z�B
}VB
�B
��B
�GB
�{B
�3B
�B
�9B
�mB
�9B
��B
�YB
��B
�#B
��B
�B
�TB
��B
�&B
��B
�B
�
B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�!B
��B
�B
�HB
��B
�&B
�ZB
��B
��B
�>B
�DB
�eB
��B
�kB
�qB
��B
��B
��B
��B
��B
�vB
��B
�3B
��B
��B
��B
�tB
��B
�fB
��B
�RB
��B
��B
�*B
��B
��B
�0B
��B
��B
�B
�]B
��B
��B
��B
� B
�B
��B
ªB
ªB
�B
��B
ĜB
��B
�tB
�EB
�_B
ȀB
�RB
�=B
��B
��B
�0B
̘B
͹B
�pB
ΊB
ϑB
�.B
бB
�4B
� B
�TB
�oB
�oB
҉B
�oB
�oB
�oB
ңB
�&B
�[B
өB
ԕB
�mB
��B
�?B
�?B
׍B
�EB
��B
�1B
�B
ٴB
ٴB
��B
ٴB
�7B
چB
�	B
یB
�xB
ܬB
��B
��B
��B
ݘB
��B
�B
ޞB
ޞB
�!B
��B
��B
��B
�B
��B
�4B
�B
�NB
�B
��B
��B
�B
�B
�ZB
�tB
�B
��B
��B
�LB
�B
��B
��B
��B
�B
�B
�RB
�B
�>B
�sB
��B
�B
�_B
�B
�B
�B
�B
�B
�B
�B
�QB
�6B
�qB
��B
��B
��B
�B
�wB
��B
�B
�5B
� B
� B
�OB
��B
�B
�;B
�oB
��B
�'B
�vB
�[B
�vB
�B
�B
�B
�TB
��B
�ZB
��B
��B
��B
��B
�B
�+B
�`B
�`B
�zB
�zB
�zB
��B
��B
�LB
��B
��B
��B
��B
�B
��B
��B
��B
�B
�0B
�0B
�0B
��B
��B
��B
�B
��B
�"B
�VB
�qB
�qB
��B
�B
�B
�wB
�cB
�}B
��B OB 4B �B �B B�BBuB�B�BB-B�B�B�B3B�B�B�B�B�BB�BYB�B�BB_BzB�B�B1B�B�B�B�B	7B	�B	�B	�B	�B
#B
rB
�B
�B
�B
�B^B�B�B0BdB�B�BBB6BPB�B<B�B�B�BB�BBvB�B�B�B.BbB�B�B B BNB�B�BBTB�B�BB�B,B�BgBMBMB�B�B�B�B�B�B�B�BBBSBmB�B�B�B_ByBKB�B�B�BBB7BkBQB�B�B=B�B�B�B�B�B�B�BBjB�B�B;BVB�B B!HB!|B!bB!�B"B"�B"�B"�B"�B#B#B#:B#�B$B$&B$�B$�B$�B%,B%B%,B%�B&2B&2B&fB&�B'8B'�B'�B(�B)*B)�B)�B*0B*KB*KB*eB*�B*�B+QB+kB+�B+�B+�B,"B-B-)B-wB-�B-�B-�B.�B.�B/ B/ B/�B/�B/�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230422094155  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230422094156  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230422094156  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230422094157                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230422094157  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230422094157  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20230422095722                      G�O�G�O�G�O�                