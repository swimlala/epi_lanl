CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T10:12:45Z creation      
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
resolution        =���   axis      Z        P  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     P  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  o   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     P  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181121101245  20181121101245  5904057 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               cA   AO  5006                            2B  A   NAVIS_A                         0305                            082713                          863 @�٣��v�1   @�٤W:�@7��C���c�333331   GPS     Primary sampling: mixed [deep: discrete, shallow: continuous]                                                                                                                                                                                                      cA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D��D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�C3DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D��3D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�<�DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ DԼ�D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�<�D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�0 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�
>@�=q@�=qA�A=�A]�A}�A��\A��\A��\A��\AΏ\Aޏ\A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C�RC��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]�C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs�Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�Dt{D�{Dt{D�{Dt{D�{DnD�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOnDO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYz�DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|t{D|�{D}t{D}�{D~t{D~�{Dt{D�{D�:=D�z=D��
D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��pD�=pD�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��
D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��
D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=Dº=D��=D�=pD�z=Dú=D��=D�:=D�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�z=DƽpD��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�7
D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�:=D�z=DԷ
D��=D�:=D�z=Dպ=D��=D�:=D�z=Dֺ=D��=D�:=D�z=D׺=D��=D�:=D�z=Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�7
D�z=Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��=D�:=D�z=Dߺ=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��pD�*=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Ạ�A̩�A̶FA̸RA̲-A�|�A�ffA�S�A�O�A�M�A�K�A�I�A�K�A�M�A�O�A�Q�A�S�A�Q�A�O�A�O�A�Q�A�M�A�G�A�C�A�=qA�7LA�5?A�5?A�5?A�7LA�7LA�;dA�;dA�;dA�9XA�9XA�9XA�=qA�;dA�=qA�A�A�+Aˡ�A�%A�7LA�S�AÙ�A���A�$�A�$�A�I�A�E�A�  A�-A��A�r�A�"�A���A�Q�A�S�A�VA��
A��RA�Q�A��!A�9XA��yA�O�A���A��PA�
=A�JA�"�A��A�dZA���A�A�dZA��hA���A�bNA���A�Q�A�33A�oA��^A�A���A��yA���A���A��!A��A�ȴA��A�n�A�&�A�^5A�n�A��A�O�A�~�A�1'A�oA��RA���A���A�+A��A��A�(�A�;dA���A� �A�VA�XA�A��yA���A�JA���A���A�-A�M�A�7A~~�A|v�Ay��Ax�Aw��As��ArffAr(�Aq�7Ap�\Aot�An��Al��Aj��AihsAf�+Ac��A`A]�;A\{A[33AYƨAX��AW`BAU&�AR1'APJANffAM�AMx�AL�ALQ�AK�hAJv�AI��AHAF��AF�+AF5?AE�^AD��ACAB�A@�A>A<��A<bA:��A9t�A9"�A8�yA7t�A5A57LA4�\A3p�A21'A133A0�9A0-A//A-�7A*��A(=qA'?}A&^5A#��A"�uA!��A!&�A z�A {A�RA^5A1A�hA�7A%AA�A-A��AA�AA��A��A�
AG�A��A�A�A�hAhsA
�A
E�A	�PA	"�A�yAȴA�\Az�A�#A+A~�A(�A��AdZA�AO�A%AjA�AA@��@�V@�?}@��@�|�@��@�F@��@�`B@�l�@�`B@띲@�G�@�{@�V@�A�@�@�j@ߍP@���@�x�@��/@�dZ@٩�@�r�@�+@ա�@�r�@�"�@�=q@��@�dZ@��#@̃@���@��@��@���@�E�@őh@�b@�l�@�=q@���@��@���@��m@��@���@��@�|�@��H@��T@��@�(�@�dZ@�33@�^5@��@���@��w@��!@�@���@��P@���@���@�O�@��`@��D@�1@��@��@���@�V@�@��@���@��D@�z�@�j@�Z@�1'@���@��H@�ff@�{@�O�@��D@���@�K�@��@��7@��D@�V@���@���@�G�@���@���@���@�Q�@�(�@��
@�K�@��H@�=q@��@���@�`B@��@��;@���@���@��@�;d@���@�^5@��@��T@��^@�x�@�/@�%@��u@�9X@�(�@� �@��@�b@��m@�\)@�
=@��@���@�E�@��@��^@���@�O�@��@��9@�bN@�1'@��w@�l�@�;d@��y@��+@�n�@�V@�M�@�=q@�5?@�{@���@��@�Z@���@��F@��F@���@��P@���@���@�@��@���@��9@�r�@��
@�;d@�ȴ@���@�M�@�@�p�@��`@��j@�I�@���@��w@�33@�@��@��\@�=q@�=q@�=q@�=q@�$�@�$�@���@��-@��7@�O�@��@���@��@��`@��j@�  @��;@��
@�ƨ@���@��@�l�@�S�@�+@��H@��R@��+@�ff@�ff@�V@�M�@�E�@�-@�{@��@���@��h@��7@��@�hs@�?}@�/@�&�@���@��`@���@���@���@���@���@���@��/@��9@��u@�Z@�b@�@�@�;@��@�w@
=@~v�@~V@~{@}��@}�T@}/@|��@|z�@{��@{C�@z�@zn�@y��@y�^@y�7@yhs@yx�@y��@y��@y�7@yx�@yG�@y�@xr�@w�@w�@w��@w��@w��@w|�@w|�@w|�@w\)@w
=@v�@vV@v$�@u�@u�T@u�@u��@u?}@uV@t�@tI�@t(�@s�
@s�@r��@q�@q��@q&�@pĜ@p�u@pbN@pA�@p1'@o�@o��@n�R@m�@m��@mV@lz�@l1@k�F@kt�@j�H@j�\@j�@i�7@iG�@h��@hQ�@hb@g��@g+@f��@f{@e@eO�@e�@c�
@b�@b�\@bM�@bJ@ax�@`��@`�`@`�`@`��@` �@^��@^��@^v�@]�T@]?}@]/@]V@\�/@\�@\j@\I�@\�@[C�@Z�!@Z�\@ZM�@Y�^@YG�@X��@X�9@Xr�@XQ�@XA�@X  @W�;@V�@V��@VE�@V@U�-@U�h@UV@T9X@S�m@S�F@St�@SC�@SC�@S33@S33@S33@S"�@R�@R�H@R��@R��@R��@R��@R��@R^5@Q��@Q��@Qhs@QG�@Q%@P��@PbN@O�@O�@NV@N{@M��@M�h@MV@L�/@L�@L�@K"�@K@J��@J�\@J�!@J�H@J�H@J�@J�\@J�@I�^@I��@Ihs@I&�@H��@H��@H��@H��@H�9@H �@G�;@G�w@G��@F��@Fv�@FE�@F$�@EO�@D�j@D�D@DZ@D9X@D�@C�
@CdZ@B��@B^5@A�^@AX@A&�@@Ĝ@@A�@?��@?K�@>��@>�@>�@>��@>5?@=��@=�@=?}@<�@<Z@;�m@;��@;C�@;@:��@:��@:~�@:^5@:-@9��@9��@9�@9��@9�^@9��@9hs@9&�@8�u@8bN@8  @7|�@7
=@6�@6��@65?@5�-@5@5�-@5��@5�h@5p�@5/@4��@4�@3��@3�
@3t�@3C�@2�!@2�@1�#@1��@1��@1��@1�^@1�7@1hs@0��@0  @/��@/�P@/l�@/+@.�R@.��@.V@-�-@-�@-`B@-p�@-`B@-�@,9X@+��@+"�@*�!@*n�@*J@)�#@)��@)��@)��@)x�@)G�@(��@(Q�@(A�@(b@'�@'�;@'��@'�@'K�@&��@&��@&v�@&v�@&ff@&ff@&ff@&$�@%�@%�T@%��@%p�@%?}@$�/@$�@$9X@#�m@#�m@#�m@#�
@#ƨ@#��@#��@#t�@#S�@#@"��@"^5@"=q@"-@"J@!�@!�^@!��@!�7@!X@!G�@!G�@!7L@!&�@!�@!%@ �`@ Ĝ@ �u@ �@  �@�w@��@�P@�P@;d@
=@��@��@$�@��@�@O�@?}@?}@V@�j@�@�D@z�@Z@�F@dZ@C�@C�@33@��@�\@�\@~�@=q@�@�#@�^@x�@X@G�@&�@�`@r�@A�@�@��@l�@�@ȴ@��@��@��@ff@$�@�@`B@O�@/@�@�/@j@9X@�@1@�m@�F@��@S�@@��@�!@-@��@��@x�@7L@%@��@��@b@�@�w@
=@�+@V@5?@�@@�-@�-@��@p�@`B@�@�@V@��@�D@j@I�@�m@t�@33@
��@
n�@
�@
J@	�#@	�^@	x�@	&�@�9@�u@�@r�@Q�@1'@  @�;@��@�w@�w@�@��@\)@+@�@�R@v�@ff@E�@E�@{@�@��@@�-@��@�h@`B@�@��@�j@j@�@��@�
@ƨ@�F@��@�@t�@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Ạ�A̩�A̶FA̸RA̲-A�|�A�ffA�S�A�O�A�M�A�K�A�I�A�K�A�M�A�O�A�Q�A�S�A�Q�A�O�A�O�A�Q�A�M�A�G�A�C�A�=qA�7LA�5?A�5?A�5?A�7LA�7LA�;dA�;dA�;dA�9XA�9XA�9XA�=qA�;dA�=qA�A�A�+Aˡ�A�%A�7LA�S�AÙ�A���A�$�A�$�A�I�A�E�A�  A�-A��A�r�A�"�A���A�Q�A�S�A�VA��
A��RA�Q�A��!A�9XA��yA�O�A���A��PA�
=A�JA�"�A��A�dZA���A�A�dZA��hA���A�bNA���A�Q�A�33A�oA��^A�A���A��yA���A���A��!A��A�ȴA��A�n�A�&�A�^5A�n�A��A�O�A�~�A�1'A�oA��RA���A���A�+A��A��A�(�A�;dA���A� �A�VA�XA�A��yA���A�JA���A���A�-A�M�A�7A~~�A|v�Ay��Ax�Aw��As��ArffAr(�Aq�7Ap�\Aot�An��Al��Aj��AihsAf�+Ac��A`A]�;A\{A[33AYƨAX��AW`BAU&�AR1'APJANffAM�AMx�AL�ALQ�AK�hAJv�AI��AHAF��AF�+AF5?AE�^AD��ACAB�A@�A>A<��A<bA:��A9t�A9"�A8�yA7t�A5A57LA4�\A3p�A21'A133A0�9A0-A//A-�7A*��A(=qA'?}A&^5A#��A"�uA!��A!&�A z�A {A�RA^5A1A�hA�7A%AA�A-A��AA�AA��A��A�
AG�A��A�A�A�hAhsA
�A
E�A	�PA	"�A�yAȴA�\Az�A�#A+A~�A(�A��AdZA�AO�A%AjA�AA@��@�V@�?}@��@�|�@��@�F@��@�`B@�l�@�`B@띲@�G�@�{@�V@�A�@�@�j@ߍP@���@�x�@��/@�dZ@٩�@�r�@�+@ա�@�r�@�"�@�=q@��@�dZ@��#@̃@���@��@��@���@�E�@őh@�b@�l�@�=q@���@��@���@��m@��@���@��@�|�@��H@��T@��@�(�@�dZ@�33@�^5@��@���@��w@��!@�@���@��P@���@���@�O�@��`@��D@�1@��@��@���@�V@�@��@���@��D@�z�@�j@�Z@�1'@���@��H@�ff@�{@�O�@��D@���@�K�@��@��7@��D@�V@���@���@�G�@���@���@���@�Q�@�(�@��
@�K�@��H@�=q@��@���@�`B@��@��;@���@���@��@�;d@���@�^5@��@��T@��^@�x�@�/@�%@��u@�9X@�(�@� �@��@�b@��m@�\)@�
=@��@���@�E�@��@��^@���@�O�@��@��9@�bN@�1'@��w@�l�@�;d@��y@��+@�n�@�V@�M�@�=q@�5?@�{@���@��@�Z@���@��F@��F@���@��P@���@���@�@��@���@��9@�r�@��
@�;d@�ȴ@���@�M�@�@�p�@��`@��j@�I�@���@��w@�33@�@��@��\@�=q@�=q@�=q@�=q@�$�@�$�@���@��-@��7@�O�@��@���@��@��`@��j@�  @��;@��
@�ƨ@���@��@�l�@�S�@�+@��H@��R@��+@�ff@�ff@�V@�M�@�E�@�-@�{@��@���@��h@��7@��@�hs@�?}@�/@�&�@���@��`@���@���@���@���@���@���@��/@��9@��u@�Z@�b@�@�@�;@��@�w@
=@~v�@~V@~{@}��@}�T@}/@|��@|z�@{��@{C�@z�@zn�@y��@y�^@y�7@yhs@yx�@y��@y��@y�7@yx�@yG�@y�@xr�@w�@w�@w��@w��@w��@w|�@w|�@w|�@w\)@w
=@v�@vV@v$�@u�@u�T@u�@u��@u?}@uV@t�@tI�@t(�@s�
@s�@r��@q�@q��@q&�@pĜ@p�u@pbN@pA�@p1'@o�@o��@n�R@m�@m��@mV@lz�@l1@k�F@kt�@j�H@j�\@j�@i�7@iG�@h��@hQ�@hb@g��@g+@f��@f{@e@eO�@e�@c�
@b�@b�\@bM�@bJ@ax�@`��@`�`@`�`@`��@` �@^��@^��@^v�@]�T@]?}@]/@]V@\�/@\�@\j@\I�@\�@[C�@Z�!@Z�\@ZM�@Y�^@YG�@X��@X�9@Xr�@XQ�@XA�@X  @W�;@V�@V��@VE�@V@U�-@U�h@UV@T9X@S�m@S�F@St�@SC�@SC�@S33@S33@S33@S"�@R�@R�H@R��@R��@R��@R��@R��@R^5@Q��@Q��@Qhs@QG�@Q%@P��@PbN@O�@O�@NV@N{@M��@M�h@MV@L�/@L�@L�@K"�@K@J��@J�\@J�!@J�H@J�H@J�@J�\@J�@I�^@I��@Ihs@I&�@H��@H��@H��@H��@H�9@H �@G�;@G�w@G��@F��@Fv�@FE�@F$�@EO�@D�j@D�D@DZ@D9X@D�@C�
@CdZ@B��@B^5@A�^@AX@A&�@@Ĝ@@A�@?��@?K�@>��@>�@>�@>��@>5?@=��@=�@=?}@<�@<Z@;�m@;��@;C�@;@:��@:��@:~�@:^5@:-@9��@9��@9�@9��@9�^@9��@9hs@9&�@8�u@8bN@8  @7|�@7
=@6�@6��@65?@5�-@5@5�-@5��@5�h@5p�@5/@4��@4�@3��@3�
@3t�@3C�@2�!@2�@1�#@1��@1��@1��@1�^@1�7@1hs@0��@0  @/��@/�P@/l�@/+@.�R@.��@.V@-�-@-�@-`B@-p�@-`B@-�@,9X@+��@+"�@*�!@*n�@*J@)�#@)��@)��@)��@)x�@)G�@(��@(Q�@(A�@(b@'�@'�;@'��@'�@'K�@&��@&��@&v�@&v�@&ff@&ff@&ff@&$�@%�@%�T@%��@%p�@%?}@$�/@$�@$9X@#�m@#�m@#�m@#�
@#ƨ@#��@#��@#t�@#S�@#@"��@"^5@"=q@"-@"J@!�@!�^@!��@!�7@!X@!G�@!G�@!7L@!&�@!�@!%@ �`@ Ĝ@ �u@ �@  �@�w@��@�P@�P@;d@
=@��@��@$�@��@�@O�@?}@?}@V@�j@�@�D@z�@Z@�F@dZ@C�@C�@33@��@�\@�\@~�@=q@�@�#@�^@x�@X@G�@&�@�`@r�@A�@�@��@l�@�@ȴ@��@��@��@ff@$�@�@`B@O�@/@�@�/@j@9X@�@1@�m@�F@��@S�@@��@�!@-@��@��@x�@7L@%@��@��@b@�@�w@
=@�+@V@5?@�@@�-@�-@��@p�@`B@�@�@V@��@�D@j@I�@�m@t�@33@
��@
n�@
�@
J@	�#@	�^@	x�@	&�@�9@�u@�@r�@Q�@1'@  @�;@��@�w@�w@�@��@\)@+@�@�R@v�@ff@E�@E�@{@�@��@@�-@��@�h@`B@�@��@�j@j@�@��@�
@ƨ@�F@��@�@t�@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BN�BN�BN�BN�BM�BK�BJ�BI�BI�BI�BJ�BL�BL�BL�BM�BM�BN�BP�BQ�BQ�BR�BS�BR�BR�BQ�BP�BP�BP�BQ�BQ�BQ�BR�BR�BR�BR�BR�BR�BS�BT�BW
B[#B^5BiyB{�B{�B�B�B�B�1B�PB�hB��B��B�B�^B�dB�wB�wB�}B�FB�B��B��B��B��B�B�'B�3B��B��B�uB�=B}�Bu�Bz�B��B��B�hB�Bn�BS�B8RB33B0!B-B�BVB%B��B�B�B�B��BɺBǮB��B�LB��Bt�B\)BG�BK�BO�BP�BcTBx�Bs�Bm�B_;BF�B.B#�B�B
��B
�fB
�)B
�B
ĜB
�wB
�!B
��B
�uB
�=B
~�B
�=B
�DB
w�B
]/B
R�B
A�B
�B
uB
hB
VB
�B
{B
JB	��B	�B	�HB	ǮB	�B	�bB	�B	�B	z�B	o�B	jB	ZB	C�B	'�B	�B	1B	%B		7B	oB	�B	oB	+B	B��B�B�B�B�B�ZB�B��BB�LB�!B�B��B�B�B��B��B��B��B��B��B�uB�\B�DB�1B�B}�Bx�Bt�Bp�Bl�Bt�Bq�BjBgmBcTB_;B\)B^5B^5B`BBcTBdZBgmBffBcTB`BB^5B[#BXBT�BR�BP�BN�BK�BD�BA�BA�B@�B?}B>wB=qB<jB<jB;dB:^B8RB7LB6FB5?B49B2-B1'B0!B/B.B-B,B)�B'�B%�B$�B#�B"�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B#�B#�B$�B$�B$�B#�B#�B%�B%�B%�B&�B&�B(�B(�B)�B,B2-B5?B6FB6FB6FB5?B5?B5?B6FB7LB9XB;dB<jB>wB@�BD�BG�BE�BE�BL�BQ�BR�BR�BS�BT�BVBW
BZBaHBffBgmBm�Bv�Bv�Bu�Bu�Bu�Bu�Bv�Bw�B� B�%B�PB�uB��B��B��B��B��B��B��B��B�B�B�'B�-B�3B�FB�LB�XB�qB��BƨBɺB��B��B�B�)B�)B�)B�/B�BB�ZB�mB�yB�B�B�B�B�B��B��B��B��B��B��B	  B	%B	
=B	DB	PB	\B	uB	�B	�B	�B	�B	�B	"�B	$�B	&�B	(�B	(�B	+B	,B	-B	-B	-B	-B	-B	-B	.B	49B	8RB	;dB	=qB	>wB	?}B	?}B	D�B	O�B	P�B	Q�B	VB	XB	ZB	]/B	bNB	e`B	ffB	hsB	k�B	l�B	n�B	n�B	q�B	s�B	t�B	x�B	z�B	z�B	|�B	~�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�1B	�7B	�7B	�=B	�bB	�hB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�9B	�9B	�?B	�FB	�XB	�jB	��B	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�5B	�5B	�;B	�HB	�NB	�ZB	�ZB	�ZB	�fB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
1B
1B

=B
DB
DB
DB
DB
JB
PB
PB
PB
PB
VB
bB
bB
bB
hB
oB
oB
oB
oB
oB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
)�B
)�B
)�B
+B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
,B
,B
,B
,B
-B
.B
.B
-B
/B
/B
/B
/B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
B�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
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
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BN�BN�BN�BN�BM�BK�BJ�BI�BI�BI�BJ�BL�BL�BL�BM�BM�BN�BP�BQ�BQ�BR�BS�BR�BR�BQ�BP�BP�BP�BQ�BQ�BQ�BR�BR�BR�BR�BR�BR�BS�BT�BW
B[#B^5BiyB{�B{�B�B�B�B�1B�PB�hB��B��B�B�^B�dB�wB�wB�}B�FB�B��B��B��B��B�B�'B�3B��B��B�uB�=B}�Bu�Bz�B��B��B�hB�Bn�BS�B8RB33B0!B-B�BVB%B��B�B�B�B��BɺBǮB��B�LB��Bt�B\)BG�BK�BO�BP�BcTBx�Bs�Bm�B_;BF�B.B#�B�B
��B
�fB
�)B
�B
ĜB
�wB
�!B
��B
�uB
�=B
~�B
�=B
�DB
w�B
]/B
R�B
A�B
�B
uB
hB
VB
�B
{B
JB	��B	�B	�HB	ǮB	�B	�bB	�B	�B	z�B	o�B	jB	ZB	C�B	'�B	�B	1B	%B		7B	oB	�B	oB	+B	B��B�B�B�B�B�ZB�B��BB�LB�!B�B��B�B�B��B��B��B��B��B��B�uB�\B�DB�1B�B}�Bx�Bt�Bp�Bl�Bt�Bq�BjBgmBcTB_;B\)B^5B^5B`BBcTBdZBgmBffBcTB`BB^5B[#BXBT�BR�BP�BN�BK�BD�BA�BA�B@�B?}B>wB=qB<jB<jB;dB:^B8RB7LB6FB5?B49B2-B1'B0!B/B.B-B,B)�B'�B%�B$�B#�B"�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B#�B#�B$�B$�B$�B#�B#�B%�B%�B%�B&�B&�B(�B(�B)�B,B2-B5?B6FB6FB6FB5?B5?B5?B6FB7LB9XB;dB<jB>wB@�BD�BG�BE�BE�BL�BQ�BR�BR�BS�BT�BVBW
BZBaHBffBgmBm�Bv�Bv�Bu�Bu�Bu�Bu�Bv�Bw�B� B�%B�PB�uB��B��B��B��B��B��B��B��B�B�B�'B�-B�3B�FB�LB�XB�qB��BƨBɺB��B��B�B�)B�)B�)B�/B�BB�ZB�mB�yB�B�B�B�B�B��B��B��B��B��B��B	  B	%B	
=B	DB	PB	\B	uB	�B	�B	�B	�B	�B	"�B	$�B	&�B	(�B	(�B	+B	,B	-B	-B	-B	-B	-B	-B	.B	49B	8RB	;dB	=qB	>wB	?}B	?}B	D�B	O�B	P�B	Q�B	VB	XB	ZB	]/B	bNB	e`B	ffB	hsB	k�B	l�B	n�B	n�B	q�B	s�B	t�B	x�B	z�B	z�B	|�B	~�B	~�B	~�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�1B	�7B	�7B	�=B	�bB	�hB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�9B	�9B	�?B	�FB	�XB	�jB	��B	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�;B	�5B	�5B	�;B	�HB	�NB	�ZB	�ZB	�ZB	�fB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
1B
1B
1B

=B
DB
DB
DB
DB
JB
PB
PB
PB
PB
VB
bB
bB
bB
hB
oB
oB
oB
oB
oB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
)�B
)�B
)�B
+B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
,B
,B
,B
,B
,B
,B
-B
.B
.B
-B
/B
/B
/B
/B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
B�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
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
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181121101245                              AO  ARCAADJP                                                                    20181121101245    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121101245  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121101245  QCF$                G�O�G�O�G�O�0               