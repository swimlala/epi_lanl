CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-01-14T10:01:20Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        |  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  `(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �      TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȱ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ̐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220114100120  20220114100120  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @ٱ����1   @ٱ֨�B.@<����F�c�"��`B1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @���@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�
>@�=q@�=qA�A=�A]�A}�A�A��\A��\A��\AΏ\Aޏ\A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D��D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|t{D|�{D}t{D}�{D~t{D~�{Dt{D�{D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�w
D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=Dº=D��=D�:=D�z=Dú=D��=D�:=D�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�z=Dƺ=D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�:=D�z=DԺ=D��=D�:=D�z=Dպ=D��=D�:=D�z=Dֺ=D��=D�:=D�z=D׺=D��=D�:=D�z=Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�:=D�z=Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��=D�:=D�z=Dߺ=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��pD�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��pD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�%A�1A�1A�
=A�
=A�
=A�JA�VA�VA�VA�
=A�VA�%A�A�A�A���A��A��yA��A��A��yA��yA��mA��mA��TA��;A��;A��#A���A���A���A���A�A��jA��RA��^A��^A��9A���A��uA��A�p�A�ZA�`BA�Q�A�=qA�5?A�A���A��-A���A��TA��DA��jA�^5A��A�$�A�S�A��PA�v�A���A�ffA�5?A� �A�bNA��mA���A�VA�A�A���A�M�A���A�x�A�
=A��DA��A�1'A�l�A�{A�E�A�+A�%A��A���A�^5A���A���A���A�(�A��/A���A��A��FA�bNA�$�A���A��uA�oA�1'A}��A{�mAzr�Ay33Ax�Avn�Au
=At�Ar��Aq�#ApM�Ao��Ao%Am\)Al�!Ak��Aj�uAi�#Ag��Ae�#AeVAdbAa�A`�A_+A]�PA\�!AZ�`AYƨAX��AV�uAU
=AT=qAR�jARI�AQ+AP9XANbNAM|�AL�AK�-AK��AK��AJ�AI�hAH��AG;dAF�uAF1AD�/AC+AB �A@ȴA?��A>E�A=\)A<v�A;VA9��A9|�A9S�A8��A8  A7��A77LA6^5A5�hA5A4-A1��A09XA/��A/�A/?}A.�`A.v�A.1'A.1A-��A-"�A,��A,r�A,�A*z�A(��A';dA%dZA#�A#�A#A"�A"VA"=qA"1A"A!�^A �A �A VA bAhsA^5AA�hA�yA~�AffA9XA�Al�A��AĜA�#A
=A�+A�wA�HAv�A9XA�
A�AXA�FAO�A�A  A`BA
=AJA"�A�uA-AƨA�PA?}A�HA=qA��A|�A%A
~�A
{A	��A	VA��A�mA�A�A$�A�A7LA�AA�A{AS�A�A�TA ȴ@���@�v�@��/@��;@�ff@���@���@�o@�G�@�1@���@�+@��@�\@�J@��@��/@�@��@��/@�j@�z�@�dZ@ޗ�@�?}@���@�X@�z�@���@�O�@��`@���@ԛ�@�bN@�{@��@�Q�@ʧ�@��T@�`B@���@Ȭ@�z�@�1@Ɨ�@�x�@ċD@+@��-@���@�I�@���@�V@�7L@���@���@�I�@�ȴ@��@�|�@�
=@���@��-@��7@�hs@�%@�r�@���@�hs@�Ĝ@�Z@�b@���@���@�~�@�@���@���@�`B@�G�@�Ĝ@�A�@�  @�@��!@�{@���@�`B@��D@�1'@���@�K�@���@���@��@�G�@�I�@��@�ƨ@�|�@�\)@�ȴ@�hs@��@�j@�b@�ƨ@�l�@��\@�J@�O�@���@�9X@�\)@���@���@�v�@�5?@�{@���@��j@��@�t�@�S�@�C�@���@��!@�~�@��@���@��@���@��`@�1'@��
@���@�l�@�;d@��y@��+@�E�@�J@��T@���@��h@�G�@��/@���@��u@�j@�  @���@�\)@�;d@�33@��@�
=@��+@�$�@���@��-@��@�hs@��@��u@�j@�9X@�b@�1@��;@��w@��P@�33@��@���@��@�x�@�?}@���@�r�@� �@��w@���@�\)@��@��@�V@�5?@�@�`B@�%@��@�bN@�(�@��F@��P@�l�@���@�ȴ@��+@�J@��@���@�7L@�V@��/@�r�@�(�@��@�1@�;@�P@~�R@~ff@}�T@}?}@|��@|9X@{��@{��@{��@{�
@{��@z�H@zJ@y�^@yx�@yG�@y7L@y�@y�@y%@x�`@x��@x1'@w;d@v��@v�+@vV@v5?@v@u`B@s�F@r�@r��@r��@r=q@r-@r�@q��@q��@q�7@qG�@q%@p�u@p1'@p �@pb@o�w@o��@o\)@oK�@oK�@oK�@nE�@m�@m�@m��@m�-@mO�@l��@l9X@kƨ@kdZ@k"�@j�H@j�!@j��@j�\@j^5@i��@i�^@ix�@iG�@i&�@h��@hĜ@h  @g�@gl�@f�@f{@ep�@eO�@d��@d�j@d��@dI�@c�m@c�F@c�@co@b��@b�@a��@a�7@a&�@`Ĝ@`r�@_�;@_�@_|�@_\)@_K�@_�@^�R@^v�@^{@]�-@]�h@]?}@\�j@\I�@\9X@[�F@[dZ@["�@[@Z��@Z~�@ZM�@Z�@Y�#@Y��@Yx�@Yhs@Y7L@Y�@X��@X��@XQ�@X �@X  @W�;@W�;@W��@W�P@WK�@W
=@V�@V�+@V5?@V$�@V$�@V@U��@U��@UO�@T�@Tj@S�F@SS�@R��@R�@Q�^@Qhs@QX@P��@PA�@O�;@O��@O�w@O�@O�P@Ol�@O+@N�y@N�y@N�@N��@N��@N�+@N5?@M��@M��@MO�@L�/@K�m@K�@KS�@K33@K@J�!@J�@I%@HĜ@H��@HA�@G�@GK�@F�y@F�R@Fv�@FE�@F$�@F{@E�@E?}@D�@D�j@D�D@DZ@D(�@C�
@C�F@CC�@BM�@BJ@A��@A��@A�@A�#@A��@A��@AG�@@��@@�u@@ �@?��@?��@?�P@?;d@?�@?
=@>��@>ȴ@>��@>V@>{@=�T@=@=�@=O�@=V@<�@<��@<��@<�D@<��@<��@<�D@<Z@;��@;�
@;�F@;��@;C�@:��@:^5@:=q@:=q@:J@9��@9X@9%@8�u@8�@81'@7��@7l�@7�@6ȴ@6��@6�+@6v�@65?@6@5��@5�@5O�@5/@5V@4�@4�@4�D@4j@4I�@41@3�m@3��@3��@3��@3�@3�@3S�@3"�@2��@2^5@1��@1��@1�^@1��@1hs@0��@0Q�@/�@/��@/�P@/�@.��@.ȴ@.�+@.v�@.ff@.{@-@-@-�@-�@-V@,�/@,z�@,I�@+�m@+dZ@+"�@+o@+@*�@*�H@*�!@*M�@*-@*J@*J@)�#@)��@)�7@)hs@)G�@)G�@)&�@(��@(Ĝ@(r�@(Q�@'�;@&�R@&V@&5?@%�-@%`B@%?}@%/@%�@$�@$�@$z�@$9X@$1@$1@#ƨ@#�@#dZ@#S�@#"�@"�H@"�\@"=q@"-@"J@"J@"J@"J@"J@!��@!��@!�@!��@!�7@!hs@!X@!X@!%@ �9@ �@ r�@ bN@ A�@ 1'@  �@�@�;@��@|�@|�@|�@l�@\)@K�@K�@+@�y@�R@�+@ff@E�@E�@$�@�T@�-@�h@?}@V@�@�@��@�D@j@9X@ƨ@t�@C�@@��@=q@�#@hs@7L@��@Ĝ@��@�u@Q�@  @�;@�w@�@�@��@�+@ff@E�@$�@�T@`B@��@�@��@�D@z�@Z@9X@1@�m@�m@�m@ƨ@�F@��@dZ@dZ@S�@33@@��@��@n�@�@�#@�7@hs@X@X@G�@G�@7L@�@Ĝ@�@A�@b@  @�@�@�@�w@|�@;d@
=@��@�y@�y@�@�+@ff@V@5?@{@��@�h@`B@O�@?}@��@��@��@Z@9X@I�@I�@I�@I�@I�@9X@(�@�
@t�@"�@
�H@
��@
��@
�!@
~�@
M�@	��@	��@	x�@	%@��@Ĝ@�9@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�%A�1A�1A�
=A�
=A�
=A�JA�VA�VA�VA�
=A�VA�%A�A�A�A���A��A��yA��A��A��yA��yA��mA��mA��TA��;A��;A��#A���A���A���A���A�A��jA��RA��^A��^A��9A���A��uA��A�p�A�ZA�`BA�Q�A�=qA�5?A�A���A��-A���A��TA��DA��jA�^5A��A�$�A�S�A��PA�v�A���A�ffA�5?A� �A�bNA��mA���A�VA�A�A���A�M�A���A�x�A�
=A��DA��A�1'A�l�A�{A�E�A�+A�%A��A���A�^5A���A���A���A�(�A��/A���A��A��FA�bNA�$�A���A��uA�oA�1'A}��A{�mAzr�Ay33Ax�Avn�Au
=At�Ar��Aq�#ApM�Ao��Ao%Am\)Al�!Ak��Aj�uAi�#Ag��Ae�#AeVAdbAa�A`�A_+A]�PA\�!AZ�`AYƨAX��AV�uAU
=AT=qAR�jARI�AQ+AP9XANbNAM|�AL�AK�-AK��AK��AJ�AI�hAH��AG;dAF�uAF1AD�/AC+AB �A@ȴA?��A>E�A=\)A<v�A;VA9��A9|�A9S�A8��A8  A7��A77LA6^5A5�hA5A4-A1��A09XA/��A/�A/?}A.�`A.v�A.1'A.1A-��A-"�A,��A,r�A,�A*z�A(��A';dA%dZA#�A#�A#A"�A"VA"=qA"1A"A!�^A �A �A VA bAhsA^5AA�hA�yA~�AffA9XA�Al�A��AĜA�#A
=A�+A�wA�HAv�A9XA�
A�AXA�FAO�A�A  A`BA
=AJA"�A�uA-AƨA�PA?}A�HA=qA��A|�A%A
~�A
{A	��A	VA��A�mA�A�A$�A�A7LA�AA�A{AS�A�A�TA ȴ@���@�v�@��/@��;@�ff@���@���@�o@�G�@�1@���@�+@��@�\@�J@��@��/@�@��@��/@�j@�z�@�dZ@ޗ�@�?}@���@�X@�z�@���@�O�@��`@���@ԛ�@�bN@�{@��@�Q�@ʧ�@��T@�`B@���@Ȭ@�z�@�1@Ɨ�@�x�@ċD@+@��-@���@�I�@���@�V@�7L@���@���@�I�@�ȴ@��@�|�@�
=@���@��-@��7@�hs@�%@�r�@���@�hs@�Ĝ@�Z@�b@���@���@�~�@�@���@���@�`B@�G�@�Ĝ@�A�@�  @�@��!@�{@���@�`B@��D@�1'@���@�K�@���@���@��@�G�@�I�@��@�ƨ@�|�@�\)@�ȴ@�hs@��@�j@�b@�ƨ@�l�@��\@�J@�O�@���@�9X@�\)@���@���@�v�@�5?@�{@���@��j@��@�t�@�S�@�C�@���@��!@�~�@��@���@��@���@��`@�1'@��
@���@�l�@�;d@��y@��+@�E�@�J@��T@���@��h@�G�@��/@���@��u@�j@�  @���@�\)@�;d@�33@��@�
=@��+@�$�@���@��-@��@�hs@��@��u@�j@�9X@�b@�1@��;@��w@��P@�33@��@���@��@�x�@�?}@���@�r�@� �@��w@���@�\)@��@��@�V@�5?@�@�`B@�%@��@�bN@�(�@��F@��P@�l�@���@�ȴ@��+@�J@��@���@�7L@�V@��/@�r�@�(�@��@�1@�;@�P@~�R@~ff@}�T@}?}@|��@|9X@{��@{��@{��@{�
@{��@z�H@zJ@y�^@yx�@yG�@y7L@y�@y�@y%@x�`@x��@x1'@w;d@v��@v�+@vV@v5?@v@u`B@s�F@r�@r��@r��@r=q@r-@r�@q��@q��@q�7@qG�@q%@p�u@p1'@p �@pb@o�w@o��@o\)@oK�@oK�@oK�@nE�@m�@m�@m��@m�-@mO�@l��@l9X@kƨ@kdZ@k"�@j�H@j�!@j��@j�\@j^5@i��@i�^@ix�@iG�@i&�@h��@hĜ@h  @g�@gl�@f�@f{@ep�@eO�@d��@d�j@d��@dI�@c�m@c�F@c�@co@b��@b�@a��@a�7@a&�@`Ĝ@`r�@_�;@_�@_|�@_\)@_K�@_�@^�R@^v�@^{@]�-@]�h@]?}@\�j@\I�@\9X@[�F@[dZ@["�@[@Z��@Z~�@ZM�@Z�@Y�#@Y��@Yx�@Yhs@Y7L@Y�@X��@X��@XQ�@X �@X  @W�;@W�;@W��@W�P@WK�@W
=@V�@V�+@V5?@V$�@V$�@V@U��@U��@UO�@T�@Tj@S�F@SS�@R��@R�@Q�^@Qhs@QX@P��@PA�@O�;@O��@O�w@O�@O�P@Ol�@O+@N�y@N�y@N�@N��@N��@N�+@N5?@M��@M��@MO�@L�/@K�m@K�@KS�@K33@K@J�!@J�@I%@HĜ@H��@HA�@G�@GK�@F�y@F�R@Fv�@FE�@F$�@F{@E�@E?}@D�@D�j@D�D@DZ@D(�@C�
@C�F@CC�@BM�@BJ@A��@A��@A�@A�#@A��@A��@AG�@@��@@�u@@ �@?��@?��@?�P@?;d@?�@?
=@>��@>ȴ@>��@>V@>{@=�T@=@=�@=O�@=V@<�@<��@<��@<�D@<��@<��@<�D@<Z@;��@;�
@;�F@;��@;C�@:��@:^5@:=q@:=q@:J@9��@9X@9%@8�u@8�@81'@7��@7l�@7�@6ȴ@6��@6�+@6v�@65?@6@5��@5�@5O�@5/@5V@4�@4�@4�D@4j@4I�@41@3�m@3��@3��@3��@3�@3�@3S�@3"�@2��@2^5@1��@1��@1�^@1��@1hs@0��@0Q�@/�@/��@/�P@/�@.��@.ȴ@.�+@.v�@.ff@.{@-@-@-�@-�@-V@,�/@,z�@,I�@+�m@+dZ@+"�@+o@+@*�@*�H@*�!@*M�@*-@*J@*J@)�#@)��@)�7@)hs@)G�@)G�@)&�@(��@(Ĝ@(r�@(Q�@'�;@&�R@&V@&5?@%�-@%`B@%?}@%/@%�@$�@$�@$z�@$9X@$1@$1@#ƨ@#�@#dZ@#S�@#"�@"�H@"�\@"=q@"-@"J@"J@"J@"J@"J@!��@!��@!�@!��@!�7@!hs@!X@!X@!%@ �9@ �@ r�@ bN@ A�@ 1'@  �@�@�;@��@|�@|�@|�@l�@\)@K�@K�@+@�y@�R@�+@ff@E�@E�@$�@�T@�-@�h@?}@V@�@�@��@�D@j@9X@ƨ@t�@C�@@��@=q@�#@hs@7L@��@Ĝ@��@�u@Q�@  @�;@�w@�@�@��@�+@ff@E�@$�@�T@`B@��@�@��@�D@z�@Z@9X@1@�m@�m@�m@ƨ@�F@��@dZ@dZ@S�@33@@��@��@n�@�@�#@�7@hs@X@X@G�@G�@7L@�@Ĝ@�@A�@b@  @�@�@�@�w@|�@;d@
=@��@�y@�y@�@�+@ff@V@5?@{@��@�h@`B@O�@?}@��@��@��@Z@9X@I�@I�@I�@I�@I�@9X@(�@�
@t�@"�@
�H@
��@
��@
�!@
~�@
M�@	��@	��@	x�@	%@��@Ĝ@�9@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�qB�qB�jB�jB�dB�dB�^B�^B�XB�XB�^B�XB�^B�^B�^B�XB�dB�qB�wB�wB�wB�}B�}B�}B�}B�}B��B��B��B��B�}B�}B��B��B��B�}B��B��B��B��B��B��B�}B�jB��BBBÖB�qB�?B��B�7BW
B�B�`BɺB�B}�B[#BM�BJ�BD�B/B�B\BB��B��B�B�NB�B��B��BŢB�qB��B��B�hB�B{�Bq�BbNB]/B\)BYBR�BH�B=qB7LB1'B,B'�B!�B�BJB��B�B�B�yB�;B��BÖB�^B�3B�B��B��B�{B�JB�%B|�Bw�Bu�Bp�Bk�BgmB^5BYBM�BC�B=qB8RB)�B$�B�B{BbB+B+BB��B�TB�5B��B��B��BŢB��B�^B�FB�-B�9B�XB�RB�'B�B��B��B��B��B��B�bB�DB�B}�Bx�Bu�Bt�Bp�Bl�Bl�Bk�BhsBgmBe`BbNB_;B\)B^5BZBP�BO�BN�BL�BK�BI�BH�BG�BF�BE�BC�BA�B?}B:^B49B-B&�B!�B�B�B�B�B�B�B�B�B�B�B{BuBhBVBPBJB
=B	7B1B1B+B%BBBB  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�sB
�mB
�fB
�`B
�`B
�ZB
�TB
�TB
�HB
�BB
�BB
�;B
�5B
�/B
�/B
�#B
�)B
�B
�B
�B
�
B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ɺB
ǮB
ŢB
ĜB
ĜB
B
�}B
�qB
��B
�jB
�dB
�^B
�dB
�dB
�jB
�^B
�^B
�XB
�XB
�XB
�RB
�XB
�XB
�XB
�^B
�dB
�dB
�dB
�dB
�^B
�XB
�jB
�qB
��B
B
ÖB
ÖB
ĜB
ĜB
ÖB
ĜB
ǮB
ȴB
ɺB
��B
��B
��B
��B
��B
��B
�
B
�
B
�
B
�
B
�#B
�HB
�NB
�ZB
�fB
�fB
�fB
�`B
�mB
�mB
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BBBB+B1B
=BDBPBbBhB{B�B�B�B�B�B"�B#�B$�B%�B&�B(�B1'B5?B6FB9XB:^B;dB@�BC�BH�BI�BN�BS�BW
BW
BXBYBYB\)B`BBcTBffBgmBhsBk�Bk�Bl�Bq�Bs�Bv�Bw�Bw�B{�B}�B�B�B�B�B�1B�7B�DB�JB�JB�VB�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�9B�RB�^B�dB�qB�jB�wB��BBŢBȴB��B��B��B��B��B��B�
B�B�#B�/B�;B�BB�TB�`B�mB�yB�B�B�B�B��B��B��B��B��B  BB%B	7BJBPB\BuB�B�B�B�B�B�B�B �B#�B&�B(�B)�B+B)�B+B,B/B2-B49B5?B6FB6FB7LB7LB7LB7LB9XB;dB?}B@�BB�BC�BD�BE�BH�BO�BR�BS�BS�BVBVBW
BW
BYBYBZB\)B]/B_;B_;B`BBaHBbNBcTBcTBdZBe`BiyBiyBiyBjBk�Bl�Bn�Bo�Bp�Br�Br�Bs�Bt�Bt�Bt�Bu�Bv�Bw�Bx�Bx�By�By�Bz�B}�B~�B� B�B�B�+B�+B�7B�=B�=B�DB�PB�PB�VB�\B�hB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�!B�'B�-B�9B�9B�?B�FB�LB�RB�RB�XB�^B�dB�jB�jB�qB�wB�wB�wB�wB�}B��B��BBÖBĜBŢBƨBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�#B�#B�#B�/B�;B�;B�;B�;B�BB�HB�TB�ZB�`B�`B�`B�`B�fB�mB�sB�sB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBBB%B+B+B+B+B+B1B1B1B	7B	7B
=B
=B
=B
=B
=B
=BDBDBJBJBPBPBPBPBPBVB\B\BbBbBhBhBhBoBhBoBoBuBuBuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B!�B!�B!�B"�B"�B#�B#�B#�B#�B$�B$�B$�B%�B%�B%�B&�B'�B'�B'�B'�B'�B'�B'�B'�B'�B'�B'�B(�B(�B)�B(�B)�B)�B+B+B+B+B+B+B,B,B-B-B-B-B,B-B-B-B-B-B.B/B.B/B.B/B0!B0!B0!B0!B0!B1'B1'B1'B2-B2-B2-B33B33B33B49B49B5?B5?B6FB6FB7LB7LB7LB7LB8RB8RB8RB8RB9XB:^B:^B:^B:^B;dB;dB;dB<jB=qB=qB=qB=qB=qB>wB>wB>wB>wB>wB>wB?}B>wB?}B?}B?}B?}B@�B@�B@�B@�BA�BA�BB�BB�BB�BB�BB�BC�BB�BB�BC�BC�BD�BD�BD�BE�BE�BE�BE�BE�BE�BF�BF�BF�BF�BF�BG�BG�BG�BG�BH�BG�BH�BI�BI�BI�BI�BJ�BJ�BJ�BK�BK�BK�BK�BK�BK�BK�BK�BK�BL�BL�BL�BM�BM�BM�BM�BN�BN�BN�BO�BO�BP�BP�BP�BP�BQ�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B�qB�qB�jB�jB�dB�dB�^B�^B�XB�XB�^B�XB�^B�^B�^B�XB�dB�qB�wB�wB�wB�}B�}B�}B�}B�}B��B��B��B��B�}B�}B��B��B��B�}B��B��B��B��B��B��B�}B�jB��BBBÖB�qB�?B��B�7BW
B�B�`BɺB�B}�B[#BM�BJ�BD�B/B�B\BB��B��B�B�NB�B��B��BŢB�qB��B��B�hB�B{�Bq�BbNB]/B\)BYBR�BH�B=qB7LB1'B,B'�B!�B�BJB��B�B�B�yB�;B��BÖB�^B�3B�B��B��B�{B�JB�%B|�Bw�Bu�Bp�Bk�BgmB^5BYBM�BC�B=qB8RB)�B$�B�B{BbB+B+BB��B�TB�5B��B��B��BŢB��B�^B�FB�-B�9B�XB�RB�'B�B��B��B��B��B��B�bB�DB�B}�Bx�Bu�Bt�Bp�Bl�Bl�Bk�BhsBgmBe`BbNB_;B\)B^5BZBP�BO�BN�BL�BK�BI�BH�BG�BF�BE�BC�BA�B?}B:^B49B-B&�B!�B�B�B�B�B�B�B�B�B�B�B{BuBhBVBPBJB
=B	7B1B1B+B%BBBB  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�sB
�mB
�fB
�`B
�`B
�ZB
�TB
�TB
�HB
�BB
�BB
�;B
�5B
�/B
�/B
�#B
�)B
�B
�B
�B
�
B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
ɺB
ɺB
ǮB
ŢB
ĜB
ĜB
B
�}B
�qB
��B
�jB
�dB
�^B
�dB
�dB
�jB
�^B
�^B
�XB
�XB
�XB
�RB
�XB
�XB
�XB
�^B
�dB
�dB
�dB
�dB
�^B
�XB
�jB
�qB
��B
B
ÖB
ÖB
ĜB
ĜB
ÖB
ĜB
ǮB
ȴB
ɺB
��B
��B
��B
��B
��B
��B
�
B
�
B
�
B
�
B
�#B
�HB
�NB
�ZB
�fB
�fB
�fB
�`B
�mB
�mB
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��BBBB+B1B
=BDBPBbBhB{B�B�B�B�B�B"�B#�B$�B%�B&�B(�B1'B5?B6FB9XB:^B;dB@�BC�BH�BI�BN�BS�BW
BW
BXBYBYB\)B`BBcTBffBgmBhsBk�Bk�Bl�Bq�Bs�Bv�Bw�Bw�B{�B}�B�B�B�B�B�1B�7B�DB�JB�JB�VB�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�9B�RB�^B�dB�qB�jB�wB��BBŢBȴB��B��B��B��B��B��B�
B�B�#B�/B�;B�BB�TB�`B�mB�yB�B�B�B�B��B��B��B��B��B  BB%B	7BJBPB\BuB�B�B�B�B�B�B�B �B#�B&�B(�B)�B+B)�B+B,B/B2-B49B5?B6FB6FB7LB7LB7LB7LB9XB;dB?}B@�BB�BC�BD�BE�BH�BO�BR�BS�BS�BVBVBW
BW
BYBYBZB\)B]/B_;B_;B`BBaHBbNBcTBcTBdZBe`BiyBiyBiyBjBk�Bl�Bn�Bo�Bp�Br�Br�Bs�Bt�Bt�Bt�Bu�Bv�Bw�Bx�Bx�By�By�Bz�B}�B~�B� B�B�B�+B�+B�7B�=B�=B�DB�PB�PB�VB�\B�hB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�!B�'B�-B�9B�9B�?B�FB�LB�RB�RB�XB�^B�dB�jB�jB�qB�wB�wB�wB�wB�}B��B��BBÖBĜBŢBƨBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�#B�#B�#B�/B�;B�;B�;B�;B�BB�HB�TB�ZB�`B�`B�`B�`B�fB�mB�sB�sB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBBB%B+B+B+B+B+B1B1B1B	7B	7B
=B
=B
=B
=B
=B
=BDBDBJBJBPBPBPBPBPBVB\B\BbBbBhBhBhBoBhBoBoBuBuBuB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B!�B!�B!�B"�B"�B#�B#�B#�B#�B$�B$�B$�B%�B%�B%�B&�B'�B'�B'�B'�B'�B'�B'�B'�B'�B'�B'�B(�B(�B)�B(�B)�B)�B+B+B+B+B+B+B,B,B-B-B-B-B,B-B-B-B-B-B.B/B.B/B.B/B0!B0!B0!B0!B0!B1'B1'B1'B2-B2-B2-B33B33B33B49B49B5?B5?B6FB6FB7LB7LB7LB7LB8RB8RB8RB8RB9XB:^B:^B:^B:^B;dB;dB;dB<jB=qB=qB=qB=qB=qB>wB>wB>wB>wB>wB>wB?}B>wB?}B?}B?}B?}B@�B@�B@�B@�BA�BA�BB�BB�BB�BB�BB�BC�BB�BB�BC�BC�BD�BD�BD�BE�BE�BE�BE�BE�BE�BF�BF�BF�BF�BF�BG�BG�BG�BG�BH�BG�BH�BI�BI�BI�BI�BJ�BJ�BJ�BK�BK�BK�BK�BK�BK�BK�BK�BK�BL�BL�BL�BM�BM�BM�BM�BN�BN�BN�BO�BO�BP�BP�BP�BP�BQ�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220114100120                              AO  ARCAADJP                                                                    20220114100120    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220114100120  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220114100120  QCF$                G�O�G�O�G�O�8000            