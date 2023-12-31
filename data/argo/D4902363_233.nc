CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-26T00:35:47Z creation;2018-04-26T00:35:52Z conversion to V3.1;2019-12-19T07:43:51Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20180426003547  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_233                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�]��:� 1   @�]��I�@:�&��I�dh���m1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ D�|�DƼ�D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @z�H@�=q@�=qA�A=�A]�A}�A��\A��\A��\A��\AΏ\Aޏ\A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_�Bg�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D��Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|t{D|�{D}t{D}�{D~t{D~�{Dt{D�{D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�7
D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=Dº=D��=D�:=D�z=Dú=D��=D�:=D�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�w
DƷ
D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�:=D�z=DԺ=D��=D�:=D�z=Dպ=D��=D�:=D�z=Dֺ=D��=D�7
D�z=D׺=D��=D�:=D�z=Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�:=D�z=Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��=D�:=D�z=Dߺ=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��qD�
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�XA�dZA�bNA�E�A�A�A�/A��A��`A��\A�M�A�ƨA��#A�+A��A��A��/A�VA��A���A�~�A��mA�G�A��A��A�;dA�bA��yA���A���A���A�ƨA�A��RA��\A�33A���A�I�A��A��FA�A��PA�S�A�/A��A�x�A��A��yA��-A�Q�A���A��9A�7LA��#A��RA�`BA�|�A��FA�VA�A�"�A�A�A�bNA��mA�ȴA��DA���A�n�A��wA�33A���A���A�l�A�I�A�p�A���A�$�A��/A�Q�A��PA��9A�S�A�r�A�l�A��FA�E�A���A�1A��+Ax�A}�FA}x�A}?}A|��A{S�Ay`BAxA�Av��AuXAs
=Ao�FAm�^Aln�Ak��AjI�AiO�AhM�Ag|�Af1Ae�-Ac�AahsA`�\A_�FA^�uA]�mA\�A\��A\  AZ��AZE�AY`BAY&�AX��AX�jAXbAW`BAVbAS�AP1ANv�AL�/AJ�DAJ5?AI�AH��AE��AES�AD�AD��ADA�AC"�AB(�AA|�A@��A@ffA?7LA>M�A=|�A<M�A<bA<�A<A;XA;oA:z�A9�A9�A9�A9`BA8{A6^5A5��A5A5/A5VA4��A4A�A3�-A3?}A2ȴA2�+A17LA/x�A.JA,M�A+�FA+S�A*5?A(��A&��A&I�A%��A#�PA"�yA"��A"�RA"ZA"  A!�;A!x�A �/A ĜA bA�`A��A�A%A�!A�+AA�A�wA�A��AĜA��A�Av�A��AS�A;dA&�A�9A��A��AbA\)A-A�yA �A��A"�A
v�A	�TA	%A(�A��A`BA�A^5AQ�A1'A  Ar�A�\A��AƨA"�A A�@�33@���@��@���@���@�9X@��y@�&�@�  @���@��@��
@��@�o@�V@���@�u@�"�@�^5@��@�\)@��@�\@�M�@�h@���@�-@���@�t�@��y@�E�@ݩ�@�?}@ܼj@�l�@ّh@ج@�+@�V@��T@�7L@�@�dZ@���@��@̴9@˝�@ʸR@ə�@�I�@ƸR@�V@�z�@��
@+@�bN@��\@�?}@��@���@�M�@��j@���@��@��y@�v�@�$�@�@�p�@�X@��`@���@�Z@�1'@�1@�l�@���@�^5@�=q@�{@��@��-@�&�@�(�@�
=@��\@�p�@�+@���@�=q@�x�@�O�@�V@��@��m@�l�@���@�V@�5?@�p�@���@���@��@�A�@���@���@�t�@��@�J@�O�@�1'@�  @��@�K�@��H@�~�@�-@��-@�x�@�O�@��@��@�ƨ@�\)@�o@�@�/@��@��@��`@�Ĝ@��u@�j@�b@���@��@�E�@���@�I�@�b@�\)@�;d@�33@�
=@�ȴ@�=q@��@�hs@�Ĝ@��@�ƨ@��@�o@�~�@�@��T@��^@��h@���@��u@�Z@�b@��@�"�@�
=@��@���@�ff@�5?@�$�@�$�@�$�@��@���@�p�@�7L@�/@�&�@��@�V@�%@���@��/@���@���@���@�r�@�bN@� �@�1@��
@��@���@���@�t�@�S�@�33@��@���@��y@��@���@��!@���@��\@�~�@�n�@�^5@�=q@�-@��@�J@���@���@��@��@��j@���@�A�@\)@}�@}�@|�/@|�j@|�D@{��@{C�@z�@z��@z��@z=q@y��@yx�@x��@x�`@x�9@xQ�@w�;@w�P@w\)@w\)@w\)@w\)@v�R@vff@vff@vff@v{@u?}@tz�@sƨ@s�@sdZ@s"�@r��@rJ@pQ�@o;d@n��@n�+@nv�@nv�@nE�@n@m�h@mp�@m/@m/@mO�@mV@l�/@lj@k��@k��@j�H@jn�@j-@i�@i��@iX@h�`@h��@h��@h�@hA�@g��@g�P@g|�@gK�@g;d@g;d@g�@f�y@f��@f{@e��@e�@d�@d��@d9X@d(�@c��@c�F@cS�@co@b�\@a�@a&�@`��@`�9@`�u@`�@`�@`r�@`r�@`A�@_�@_
=@^V@^@]�-@\�/@\�@\�D@\j@\(�@[�
@[C�@Z�H@Z��@Z��@Z^5@ZJ@Y�#@Yhs@X�`@XĜ@X�9@X��@XA�@W�;@W�P@W;d@W+@V��@V�@Vv�@VE�@U�T@Up�@U/@U�@T��@T�j@Tz�@TZ@TI�@TI�@T(�@T�@T1@S��@S�m@S��@SC�@R�@R~�@Q��@QG�@P��@P1'@O�w@O�@N�R@N�+@NV@Mp�@L�@L�j@L�@Lz�@LI�@K�F@Ko@J��@J�!@J~�@J=q@I��@H�@Hb@G�w@G��@G��@G�P@G�P@G�P@G\)@G+@F�y@F�R@FV@E�@E�@D�j@D(�@C��@CS�@CC�@Co@B�@B�H@B��@B��@Bn�@B^5@B=q@B-@BJ@A�#@A��@A�^@A��@Ahs@A&�@@1'@@  @?�@?�w@?�@?��@?�P@?\)@?+@>��@>�y@>�@>�R@>ff@>5?@=@=p�@=?}@=�@=V@=V@=V@=V@<��@<�@<9X@;��@;ƨ@;�@;S�@;33@;"�@;@:��@:n�@:-@:�@9�@9�#@9�^@9x�@9G�@9&�@8��@8�`@8�9@8A�@8b@7�@7��@7��@7;d@6�y@6��@6V@6@5@5�@5?}@4�@4j@4�@3�m@3t�@3C�@3C�@333@2�!@2M�@2�@1�@1�#@1��@1��@1�7@1�7@1hs@1G�@0�`@0r�@0b@/��@/�@/�P@/\)@.��@.$�@.@-��@-�h@-`B@-�@,�j@,Z@,�@+�@+@*��@*n�@*^5@*^5@*n�@*M�@*J@)��@)�@)�^@)��@)�7@)7L@(�`@(��@(�@(bN@(A�@( �@(  @'l�@';d@&��@&ȴ@&ȴ@&�@&ȴ@&ȴ@&�R@&v�@%��@$�j@$I�@$�@$�@#�m@#�
@#�F@#��@#t�@#33@"�H@"-@!��@!hs@!hs@!x�@!G�@ ��@ Ĝ@ ��@ bN@ Q�@  �@ b@�;@�;@�w@�@�@�@�y@v�@V@V@E�@E�@5?@�@@�-@�h@`B@�@�/@�D@Z@��@�F@��@t�@S�@33@��@~�@n�@n�@n�@M�@J@��@�@x�@X@X@7L@��@�9@r�@1'@ �@�;@�w@��@\)@K�@�@
=@��@��@�y@ȴ@�R@��@E�@�@�h@`B@?}@��@Z@�@�m@�@S�@�@�!@�\@n�@^5@=q@-@�@�@��@�7@x�@hs@7L@��@Q�@ �@�;@�@��@�P@�P@|�@\)@+@ȴ@��@E�@{@�@�@�@�T@��@@��@V@Z@�@��@��@��@��@�@t�@S�@C�@
�@
��@
M�@
-@
�@
J@	��@	��@	&�@�9@A�@b@b@  @��@�P@+@�y@ȴ@�R@v�@5?@{@��@�h@p�@p�@p�@`B@?}@�@��@j@��@�
@�F@S�@C�@@�H@��@��@�!@~�@M�@��@�^@��@��@x�@X@ ��@ ��@ �@ r�@ r�@ bN@ bN@ bN@ Q�@ A�@ 1'@  �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�XA�dZA�bNA�E�A�A�A�/A��A��`A��\A�M�A�ƨA��#A�+A��A��A��/A�VA��A���A�~�A��mA�G�A��A��A�;dA�bA��yA���A���A���A�ƨA�A��RA��\A�33A���A�I�A��A��FA�A��PA�S�A�/A��A�x�A��A��yA��-A�Q�A���A��9A�7LA��#A��RA�`BA�|�A��FA�VA�A�"�A�A�A�bNA��mA�ȴA��DA���A�n�A��wA�33A���A���A�l�A�I�A�p�A���A�$�A��/A�Q�A��PA��9A�S�A�r�A�l�A��FA�E�A���A�1A��+Ax�A}�FA}x�A}?}A|��A{S�Ay`BAxA�Av��AuXAs
=Ao�FAm�^Aln�Ak��AjI�AiO�AhM�Ag|�Af1Ae�-Ac�AahsA`�\A_�FA^�uA]�mA\�A\��A\  AZ��AZE�AY`BAY&�AX��AX�jAXbAW`BAVbAS�AP1ANv�AL�/AJ�DAJ5?AI�AH��AE��AES�AD�AD��ADA�AC"�AB(�AA|�A@��A@ffA?7LA>M�A=|�A<M�A<bA<�A<A;XA;oA:z�A9�A9�A9�A9`BA8{A6^5A5��A5A5/A5VA4��A4A�A3�-A3?}A2ȴA2�+A17LA/x�A.JA,M�A+�FA+S�A*5?A(��A&��A&I�A%��A#�PA"�yA"��A"�RA"ZA"  A!�;A!x�A �/A ĜA bA�`A��A�A%A�!A�+AA�A�wA�A��AĜA��A�Av�A��AS�A;dA&�A�9A��A��AbA\)A-A�yA �A��A"�A
v�A	�TA	%A(�A��A`BA�A^5AQ�A1'A  Ar�A�\A��AƨA"�A A�@�33@���@��@���@���@�9X@��y@�&�@�  @���@��@��
@��@�o@�V@���@�u@�"�@�^5@��@�\)@��@�\@�M�@�h@���@�-@���@�t�@��y@�E�@ݩ�@�?}@ܼj@�l�@ّh@ج@�+@�V@��T@�7L@�@�dZ@���@��@̴9@˝�@ʸR@ə�@�I�@ƸR@�V@�z�@��
@+@�bN@��\@�?}@��@���@�M�@��j@���@��@��y@�v�@�$�@�@�p�@�X@��`@���@�Z@�1'@�1@�l�@���@�^5@�=q@�{@��@��-@�&�@�(�@�
=@��\@�p�@�+@���@�=q@�x�@�O�@�V@��@��m@�l�@���@�V@�5?@�p�@���@���@��@�A�@���@���@�t�@��@�J@�O�@�1'@�  @��@�K�@��H@�~�@�-@��-@�x�@�O�@��@��@�ƨ@�\)@�o@�@�/@��@��@��`@�Ĝ@��u@�j@�b@���@��@�E�@���@�I�@�b@�\)@�;d@�33@�
=@�ȴ@�=q@��@�hs@�Ĝ@��@�ƨ@��@�o@�~�@�@��T@��^@��h@���@��u@�Z@�b@��@�"�@�
=@��@���@�ff@�5?@�$�@�$�@�$�@��@���@�p�@�7L@�/@�&�@��@�V@�%@���@��/@���@���@���@�r�@�bN@� �@�1@��
@��@���@���@�t�@�S�@�33@��@���@��y@��@���@��!@���@��\@�~�@�n�@�^5@�=q@�-@��@�J@���@���@��@��@��j@���@�A�@\)@}�@}�@|�/@|�j@|�D@{��@{C�@z�@z��@z��@z=q@y��@yx�@x��@x�`@x�9@xQ�@w�;@w�P@w\)@w\)@w\)@w\)@v�R@vff@vff@vff@v{@u?}@tz�@sƨ@s�@sdZ@s"�@r��@rJ@pQ�@o;d@n��@n�+@nv�@nv�@nE�@n@m�h@mp�@m/@m/@mO�@mV@l�/@lj@k��@k��@j�H@jn�@j-@i�@i��@iX@h�`@h��@h��@h�@hA�@g��@g�P@g|�@gK�@g;d@g;d@g�@f�y@f��@f{@e��@e�@d�@d��@d9X@d(�@c��@c�F@cS�@co@b�\@a�@a&�@`��@`�9@`�u@`�@`�@`r�@`r�@`A�@_�@_
=@^V@^@]�-@\�/@\�@\�D@\j@\(�@[�
@[C�@Z�H@Z��@Z��@Z^5@ZJ@Y�#@Yhs@X�`@XĜ@X�9@X��@XA�@W�;@W�P@W;d@W+@V��@V�@Vv�@VE�@U�T@Up�@U/@U�@T��@T�j@Tz�@TZ@TI�@TI�@T(�@T�@T1@S��@S�m@S��@SC�@R�@R~�@Q��@QG�@P��@P1'@O�w@O�@N�R@N�+@NV@Mp�@L�@L�j@L�@Lz�@LI�@K�F@Ko@J��@J�!@J~�@J=q@I��@H�@Hb@G�w@G��@G��@G�P@G�P@G�P@G\)@G+@F�y@F�R@FV@E�@E�@D�j@D(�@C��@CS�@CC�@Co@B�@B�H@B��@B��@Bn�@B^5@B=q@B-@BJ@A�#@A��@A�^@A��@Ahs@A&�@@1'@@  @?�@?�w@?�@?��@?�P@?\)@?+@>��@>�y@>�@>�R@>ff@>5?@=@=p�@=?}@=�@=V@=V@=V@=V@<��@<�@<9X@;��@;ƨ@;�@;S�@;33@;"�@;@:��@:n�@:-@:�@9�@9�#@9�^@9x�@9G�@9&�@8��@8�`@8�9@8A�@8b@7�@7��@7��@7;d@6�y@6��@6V@6@5@5�@5?}@4�@4j@4�@3�m@3t�@3C�@3C�@333@2�!@2M�@2�@1�@1�#@1��@1��@1�7@1�7@1hs@1G�@0�`@0r�@0b@/��@/�@/�P@/\)@.��@.$�@.@-��@-�h@-`B@-�@,�j@,Z@,�@+�@+@*��@*n�@*^5@*^5@*n�@*M�@*J@)��@)�@)�^@)��@)�7@)7L@(�`@(��@(�@(bN@(A�@( �@(  @'l�@';d@&��@&ȴ@&ȴ@&�@&ȴ@&ȴ@&�R@&v�@%��@$�j@$I�@$�@$�@#�m@#�
@#�F@#��@#t�@#33@"�H@"-@!��@!hs@!hs@!x�@!G�@ ��@ Ĝ@ ��@ bN@ Q�@  �@ b@�;@�;@�w@�@�@�@�y@v�@V@V@E�@E�@5?@�@@�-@�h@`B@�@�/@�D@Z@��@�F@��@t�@S�@33@��@~�@n�@n�@n�@M�@J@��@�@x�@X@X@7L@��@�9@r�@1'@ �@�;@�w@��@\)@K�@�@
=@��@��@�y@ȴ@�R@��@E�@�@�h@`B@?}@��@Z@�@�m@�@S�@�@�!@�\@n�@^5@=q@-@�@�@��@�7@x�@hs@7L@��@Q�@ �@�;@�@��@�P@�P@|�@\)@+@ȴ@��@E�@{@�@�@�@�T@��@@��@V@Z@�@��@��@��@��@�@t�@S�@C�@
�@
��@
M�@
-@
�@
J@	��@	��@	&�@�9@A�@b@b@  @��@�P@+@�y@ȴ@�R@v�@5?@{@��@�h@p�@p�@p�@`B@?}@�@��@j@��@�
@�F@S�@C�@@�H@��@��@�!@~�@M�@��@�^@��@��@x�@X@ ��@ ��@ �@ r�@ r�@ bN@ bN@ bN@ Q�@ A�@ 1'@  �11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�;B�5B�#B�#B�#B�B��BȴBÖB�RB��B��B�B��B�-B��B�B��B��B��B��B��B��B��B��B�B�'B�?B�FB�FB�?B�'B��B��B�{B�7B� Bp�B~�Bw�B�VB�7B�By�Bv�Bx�Br�BiyB\)BH�BG�BG�B'�B'�B�B1B��B�TB��BB��B�1B�B��B��B�1By�Bp�BffBQ�BD�B&�B
=B
�B
��B
�B
�5B
ƨB
�dB
�LB
��B
�VB
�VB
�DB
�B
u�B
s�B
bNB
VB
p�B
jB
^5B
C�B
/B
5?B
"�B
�B	�B	��B	��B	�;B	�NB	�B	��B	��B	ȴB	�3B	�dB	��B	{�B	�\B	�+B	|�B	{�B	s�B	{�B	m�B	hsB	dZB	`BB	iyB	ffB	]/B	K�B	>wB	�B	bB�5B��B��B�BB	B��B�TBÖB�sB�B�yB�NB��B��B��B��BĜB�dB�RB�XB�9B��BƨBǮB�dBŢB��B�jBÖBȴBȴB�LB��B�?B�FB��B��BȴB��B�wB�jB�?B�B��B�B�7B~�B�oB�hB�Bq�B`BBr�Bm�BXBhsBs�Bq�Bl�BhsBjBe`B_;BaHBXBI�BK�BW
BW
BXB[#BS�BM�BD�B7LBF�BS�BT�B?}BS�BVB[#BS�BB�B(�B49B+B9XB49B.B<jBA�B:^B7LB9XB1'B49B:^B>wB=qB7LBB�B:^B/B�BVB)�B8RB+B#�B(�B&�B/B%�BhB�B�B�B�B �B�B"�B/B)�B'�B&�B�B�B �B�B�B&�B#�B#�B�B�B%B	7B!�B!�B�B�B�B�BuBDB�BhB�B�BVB��B  BhB�B�B�B�BoBPB\BbB�B�BbB
=B\B�B�B&�B#�B�B�B�B)�B?}BD�BF�BC�BH�BF�BJ�BH�BJ�BK�BF�BL�BN�BVBVBT�BQ�BM�BJ�BJ�BP�BK�BG�B^5BdZBaHBiyBhsBffBgmBiyBjBq�Br�Bn�Br�By�Bv�Bt�Bx�Bv�Bw�Bt�Bo�Bv�Bw�B�%B�B�%B�%B�+B�=B�1B�PB�VB�PB�=B�=B�hB�uB�VB��B��B��B��B��B��B��B��B��B��B��B��B�3B�jB�^BÖBŢBĜBÖBBǮBǮBȴB��B��B�B��B�B�)B�HB�NB�NB�BB�`B�B�B�B�B��B��B��B��B��B	B	B	B	%B	B	B	PB	uB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	'�B	'�B	'�B	(�B	)�B	-B	-B	0!B	0!B	1'B	2-B	33B	33B	49B	49B	5?B	6FB	8RB	8RB	9XB	9XB	8RB	8RB	:^B	C�B	D�B	B�B	D�B	G�B	ZB	]/B	_;B	_;B	^5B	aHB	gmB	jB	jB	jB	l�B	o�B	p�B	u�B	t�B	t�B	w�B	|�B	�B	�B	�B	�B	�B	�+B	�DB	�=B	�1B	�=B	�bB	�oB	��B	��B	��B	��B	�oB	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�?B	�LB	�LB	�qB	�}B	��B	��B	��B	ĜB	ƨB	ǮB	ǮB	ȴB	ǮB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�/B	�/B	�/B	�)B	�#B	�B	�B	�B	�5B	�;B	�5B	�TB	�ZB	�ZB	�TB	�TB	�TB	�fB	�sB	�sB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
B
B
B
B
	7B
DB
PB
VB
VB
VB
PB
JB
JB
JB
JB
JB
DB
DB
DB
VB
\B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
"�B
"�B
"�B
!�B
"�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
$�B
$�B
%�B
&�B
&�B
&�B
%�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
(�B
(�B
,B
,B
,B
.B
/B
.B
,B
-B
/B
0!B
1'B
0!B
1'B
2-B
2-B
1'B
1'B
0!B
0!B
1'B
2-B
33B
33B
2-B
0!B
2-B
5?B
5?B
5?B
49B
5?B
49B
49B
49B
33B
49B
6FB
8RB
9XB
:^B
9XB
8RB
7LB
9XB
8RB
8RB
8RB
8RB
7LB
7LB
8RB
9XB
9XB
:^B
9XB
9XB
7LB
;dB
;dB
<jB
=qB
>wB
>wB
>wB
=qB
;dB
8RB
8RB
=qB
@�B
B�B
A�B
B�B
A�B
A�B
A�B
A�B
A�B
?}B
@�B
C�B
D�B
E�B
D�B
C�B
E�B
F�B
E�B
F�B
E�B
F�B
F�B
G�B
F�B
D�B
I�B
I�B
H�B
G�B
I�B
J�B
J�B
J�B
J�B
H�B
J�B
K�B
J�B
J�B
I�B
J�B
J�B
K�B
J�B
K�B
L�B
L�B
L�B
K�B
J�B
K�B
N�B
M�B
N�B
M�B
L�B
M�B
M�B
L�B
N�B
O�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
T�B
T�B
T�B
S�B
S�B
S�B
R�B
R�B
S�B
S�B
T�B
S�B
R�B
VB
W
B
VB
W
B
W
B
XB
ZB
ZB
[#B
[#B
[#B
[#B
ZB
ZB
\)B
\)B
[#B
ZB
ZB
YB
\)B
\)B
]/B
_;B
_;B
_;B
^5B
^5B
]/B
]/B
^5B
^5B
_;B
aHB
aHB
aHB
aHB
`BB
`BB
_;B
]/B
\)B
aHB
`BB
dZB
dZB
dZB
cTB
cTB
cTB
cTB
bNB
bNB
cTB
e`B
e`B
e`B
dZB
cTB
bNB
cTB
dZB
ffB
hsB
gmB
ffB
ffB
e`B
hsB
hsB
iyB
hsB
hsB
iyB
iyB
jB
jB
l�B
l�B
m�B
l�B
l�B
l�B
iyB
k�B
n�B
n�B
m�B
o�B
o�B
p�B
p�B
q�B
p�B
p�B
p�B
o�B
p�B
r�B
r�B
q�B
q�B
p�B
q�B
q�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�!B�jB�WB�WB�qB�mB҉BɆB�gB��B�B�mB�5B�RB��B�B��B��B�B�B��B��B��B�zB�yB�}B�[B�ZB�`B�`B�tB�vB��B��B��B��B��Br�B��By	B��B��B��Bz�Bw�ByXBs�Bj�B]�BK)BIBH�B*eB)B�B
	B��B��B�9B�B��B�DB�)B��B��B��B{BrBg�BS�BFtB)�B�B
�B
ѝB
�6B
ߤB
��B
�qB
��B
� B
��B
�B
�~B
��B
w�B
u%B
dZB
W�B
p�B
kB
_!B
E�B
1�B
6�B
%B
B	��B	��B	�mB	��B	�B	��B	�FB	�BB	��B	�B	�B	�B	~�B	��B	�KB	~wB	|�B	t�B	|PB	n�B	i�B	eFB	a-B	i�B	f�B	]�B	L�B	?�B	 BB	uB��B�B��B�TB	uB��B�`B��B�B�"B�0B� B҉B�"B��B͹BżB��B��B�xB��B��B��B�B�PB�%B�uB��B�3B��B��B�$B��B��B�fB̘B�"B�7B�uB�cB�"B��B�B��B��B�)B�B�@B�TB��Bs�Bb�BshBn�BZ�BiDBs�BrBmCBiBj�BfB`BBa�BYeBKxBMBW�BW�BX�B[�BT�BN�BE�B9XBG�BT{BVBA�BT�BV�B[qBT�BC�B+�B5�B-B:�B5�B0B=qBB[B;B8lB:^B2�B5tB;0B?B>(B8BB�B:�B0B�B�B*�B8�B,"B%,B)�B'�B/�B'B[B�B �B�B�B!�B�B#�B/iB*B(�B'mB�B�B!|B�B�B'RB$tB$@B~B�B�B
�B!�B"hBVB \B;B]B�B~B?B�BEBB\B�*B�BoBdBOB�BSBuBVB}B�BIBqB�B�B�B�B�B'mB$�B	BB5B+B?�BEBF�BD3BH�BGBKBI7BKBLBG+BMPBOvBV9BVSBUMBRTBN�BK�BK�BQ�BL�BIlB^�Bd�Ba�Bi�Bh�BgBh
Bi�Bk6Bq�BsBoOBs3BzBwBu?By	Bw2Bx8BuZBp�Bw�Bx�B�tB��B��B��B��B��B��B��B��B��B��B��B��B��B�(B�EB�B�B�$B�8B�2B�2B�&B�RB��B��B��B��B��B��B��B��B��B��B�-B�B�1B�RB�jB�FB�mBՁB֡BܒB�|B�B�B��B��B��B��B�"B��B��B�	B�B�>B�.B	AB	MB	MB	?B	mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	�B	 �B	$&B	($B	($B	(>B	)*B	*KB	-CB	-]B	0UB	0UB	1[B	2aB	3hB	3hB	4nB	4nB	5tB	6zB	8�B	8�B	9�B	9�B	8�B	8�B	:�B	C�B	D�B	CB	EB	HfB	Z7B	]dB	_pB	_�B	^�B	a�B	g�B	j�B	j�B	j�B	l�B	o�B	p�B	u�B	t�B	uB	xB	}"B	�;B	�-B	�-B	�AB	�UB	�_B	�^B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�B	�B	�,B	�2B	�>B	�XB	�IB	�[B	�MB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	��B	�B	�(B	�B	� B	� B	� B	�4B	�B	� B	�[B	�gB	�KB	�WB	�]B	�IB	�IB	�IB	�]B	�WB	�eB	ؓB	چB	�jB	ߊB	ޞB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�+B	�B	�>B	�B	�0B	�(B	�.B	�(B	�PB
 OB
AB
3B
GB
AB
UB
oB
SB
YB
SB
gB
uB
�B
	lB
xB
jB
pB
pB
pB
jB
~B
~B
dB
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
�B
 �B
 �B
!�B
#B
#B
#B
!�B
#B
$B
$�B
$�B
$�B
%B
%B
%B
&B
%�B
%�B
%B
%,B
&B
'B
'B
'B
&2B
'B
'B
($B
($B
)B
)*B
*B
)*B
)DB
,=B
,=B
,WB
.IB
/5B
.IB
,WB
-CB
/OB
0UB
1AB
0UB
1AB
2GB
2GB
1[B
1[B
0oB
0UB
1[B
2aB
3MB
3hB
2aB
0�B
2|B
5tB
5tB
5tB
4nB
5tB
4�B
4�B
4nB
3�B
4�B
6zB
8�B
9rB
:xB
9rB
8�B
7fB
9rB
8�B
8�B
8�B
8�B
7�B
7�B
8�B
9�B
9�B
:�B
9�B
9rB
7�B
;�B
;�B
<�B
=�B
>�B
>�B
>�B
=�B
;�B
8�B
8�B
=�B
@�B
B�B
A�B
B�B
A�B
A�B
A�B
A�B
A�B
?�B
@�B
C�B
D�B
E�B
D�B
C�B
E�B
F�B
E�B
F�B
E�B
F�B
F�B
G�B
F�B
D�B
I�B
I�B
H�B
G�B
I�B
J�B
J�B
J�B
J�B
H�B
J�B
K�B
J�B
J�B
I�B
J�B
J�B
K�B
KB
K�B
MB
L�B
MB
K�B
KB
K�B
N�B
M�B
N�B
NB
MB
M�B
NB
MB
N�B
O�B
OB
OB
N�B
PB
PB
Q B
QB
R B
R B
R B
SB
S&B
TB
UB
UB
T�B
T,B
TB
T,B
SB
S&B
T,B
T,B
U2B
T,B
S&B
V9B
W$B
VSB
W?B
W$B
X+B
Z7B
ZQB
[=B
[WB
[=B
[=B
Z7B
Z7B
\CB
\CB
[WB
Z7B
Z7B
YKB
\]B
\]B
]IB
_VB
_VB
_VB
^jB
^jB
]dB
]~B
^jB
^jB
_pB
abB
aHB
aHB
abB
`vB
`\B
_VB
]~B
\xB
a|B
`�B
dZB
dtB
dtB
c�B
cTB
c�B
cnB
b�B
b�B
c�B
ezB
ezB
ezB
d�B
c�B
b�B
c�B
d�B
f�B
h�B
g�B
f�B
f�B
e�B
h�B
h�B
i�B
h�B
h�B
i�B
i�B
j�B
j�B
l�B
l�B
m�B
l�B
l�B
l�B
i�B
k�B
n�B
n�B
m�B
o�B
o�B
p�B
p�B
q�B
p�B
p�B
p�B
o�B
p�B
r�B
r�B
q�B
q�B
p�B
q�B
q�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.18(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804300032482018043000324820180430003248201806221241002018062212410020180622124100201804271406312018042714063120180427140631  JA  ARFMdecpA19c                                                                20180426093519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180426003547  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180426003549  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180426003550  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180426003550  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180426003550  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180426003551  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180426003551  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180426003552  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180426003552                      G�O�G�O�G�O�                JA  ARUP                                                                        20180426005831                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180426153303  CV  JULD            G�O�G�O�F��                JM  ARSQOW  1.1 2017V1                                                          20180427050631  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180429153248  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180429153248  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034100  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                