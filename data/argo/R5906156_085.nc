CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-08-12T10:00:49Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20210812100049  20210812100049  5906156 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               UA   AO  7912                            2B  A   NAVIS_A                         1020                            170425                          863 @ً�|�1   @ً��K6@90�`A�7�cE����1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         UA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�<�DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��3D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�3D�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�=q@�=qA�A=�A]�A}�A��\A��\A��\A��\AΏ\Aޏ\A�\A��\BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D��D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dz�D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|t{D|�{D}t{D}�{D~t{D~�{Dt{D�{D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�w
D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=Dº=D��=D�:=D�z=Dú=D��=D�:=D�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�z=Dƺ=D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˺=D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѺ=D��=D�:=D�z=DҺ=D��=D�7
D�z=DӺ=D��=D�:=D�z=DԺ=D��=D�:=D�z=Dպ=D��=D�:=D�z=Dֺ=D��=D�:=D�z=D׺=D��=D�:=D�z=Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�:=D�z=Dۺ=D��=D�:=D�z=Dܺ=D��=D�:=D�z=Dݺ=D��=D�:=D�z=D޺=D��=D�:=D�z=Dߺ=D��=D�:=D�z=D�pD��pD�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�pD��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�}pD��pD��pD�C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�O�A�Q�A�S�A�O�A�M�A�Q�A�K�A�M�A�I�A�9XA�A��/AʸRAʙ�AʋDA�n�A�/A�%A��A��TA��
A��
A�ƨAɺ^AɬA�E�A�oA�
=A��A��`A���A�S�A��/Aũ�A�
=A��uA���A��`A�-A�E�A�S�A� �A���A�A��A��A�t�A�"�A���A�^5A�?}A�%A�n�A���A�$�A�hsA�ƨA�O�A�{A�n�A�t�A�l�A��A���A��`A�p�A�|�A�ƨA��7A�
=A�`BA�33A��HA�ĜA���A�-A��A�-A�hsA�ȴA���A��-A�r�A�-A���A��9A�K�A���A��jA�~�A�S�A�  A��PA���A��FA���A��A���A�z�A��yA�v�A�v�A��DA���A���A���A��;A�C�A��A�&�A�?}A�I�A�%A�z�A�jA��\A�;dA���A�7LAA~~�A|��Az�!Av��At��As&�ApbNAmK�Aj��Ah��Ag��Ae\)AcO�Aa�TA_�A[\)AY�AX{ATM�ARZAQƨAQ`BAP�AM��AK�;AJ��AI��AHbNAF��AFA�AE��AEx�AC�-A?��A=�
A<^5A;C�A:Q�A9VA8$�A7x�A6 �A57LA4ffA3O�A2�A1K�A/�wA.jA-�mA-\)A+�A)�A)S�A)�A(ĜA'ƨA&��A%��A$bNA#��A#%A"  A!hsA ��Ax�A�AbNAA��Av�A��A7LA^5A�A�+A|�A�AI�AXA��A �AVAQ�AA7LA��A�mAVA��A�^Al�A
�A
Q�A	x�A��A��A �A|�A�uA��AoAJAƨA��A��A��A��A b@���@�t�@�A�@�o@�X@�
=@�%@��@�!@�&�@��;@�^5@��`@�t�@��#@�K�@�7L@�Q�@�|�@���@�@���@�9X@��
@�33@��@ى7@���@�  @�"�@�^5@�?}@�9X@��y@щ7@���@ЋD@� �@��;@�"�@���@�&�@�I�@˥�@�"�@�J@��`@�j@�C�@�v�@�@���@�Ĝ@��@��y@°!@�ff@��@�O�@���@��;@���@��7@�1@��@��!@�{@�x�@�Q�@�@���@�V@�Q�@��@�t�@���@��h@��@��m@�S�@�$�@�`B@���@���@��@�J@�&�@���@�(�@��
@�
=@���@��#@�7L@�%@�(�@���@�|�@���@�\)@�ȴ@��@��@��@��m@��w@�ƨ@��P@�;d@�o@��@�\)@�S�@��y@�M�@�{@���@�`B@��@�r�@�l�@�33@�
=@���@�^5@�=q@�J@��@��^@��@�7L@�Ĝ@��@�r�@�ƨ@�;d@���@���@�v�@�=q@��@��-@�`B@��`@��D@�r�@�(�@���@��@���@�t�@�dZ@�33@��H@���@�^5@�-@�{@��^@��@�hs@�?}@�&�@�%@��@�Ĝ@���@��u@�z�@��;@�|�@�dZ@�@��R@��!@��\@�ff@�^5@�ff@�5?@���@���@���@��^@��7@�G�@��@�V@��@��j@��D@��@�z�@�b@�ƨ@�l�@�dZ@�S�@�S�@�K�@�"�@��@���@�n�@�M�@�5?@�$�@��@���@��T@���@���@��-@��h@�hs@�7L@�7L@�%@�%@���@�Ĝ@���@�9X@��@��F@���@��@�l�@�S�@�C�@�C�@�33@��y@���@���@���@�ff@�E�@�-@�{@���@���@�p�@�?}@�%@��/@���@�Q�@�b@�1@���@�+@���@��@��y@��y@���@���@�~�@�@��T@��^@���@���@���@�hs@�&�@���@���@�j@�w@�@~�y@~��@}�T@}p�@}?}@}V@|�j@|z�@{��@{��@{C�@{@z��@z��@z��@z�!@z^5@z-@y�@y�#@y�#@y��@y��@y��@y&�@x�9@xr�@w��@wl�@w;d@v�y@vȴ@v�+@vv�@vE�@u��@uO�@t�/@t�@tI�@t(�@s��@s�@s@r�@r��@r~�@r=q@q�7@p��@q&�@p�`@p�u@pA�@o|�@n��@n$�@m�@m�@m��@mp�@m?}@l��@lz�@lZ@l9X@k��@kC�@j��@jM�@i�^@iX@h��@hĜ@h��@h�u@hbN@g��@g�;@g;d@g
=@f�y@fE�@ep�@d��@d�@dz�@d9X@c�@cC�@c"�@b��@b~�@b-@a��@a��@aX@`�u@`b@_�@_\)@_�@^�y@^ȴ@^��@^V@^$�@^@]��@]�h@]/@\��@\�@\j@\I�@\�@[��@[�
@[ƨ@[��@[C�@[o@Z��@ZJ@Y�7@Y�@XQ�@W�P@Wl�@W;d@V�y@V��@V5?@V$�@U��@UO�@U/@T�/@T�D@T�@S��@Sƨ@St�@SS�@S33@R��@R^5@R�@RJ@Q�#@Qhs@QG�@Q&�@P��@PA�@O��@O\)@N��@N�R@NV@N$�@M�T@M�@MV@L�j@Lj@K�m@Kƨ@K��@K33@Ko@Ko@K@J��@J��@J�!@J�!@J~�@J-@I�^@I��@Ix�@I7L@H�`@HĜ@H��@HbN@H �@Hb@Gl�@G�@F��@G�@G;d@G
=@F��@E�@EO�@D��@D��@DI�@DI�@C��@Ct�@C"�@Co@B=q@A��@Ahs@A7L@@��@@bN@@b@?�;@?�w@?�P@?K�@?
=@?
=@?�@>��@>5?@=�@=��@=�@<��@<�@<��@<I�@<9X@<�@;�m@;��@;C�@:�H@:~�@:�@9�@9��@9X@9�@8��@8�u@8�@8Q�@8b@7�w@7�P@7;d@7�@6�@6��@6ff@5�@5@5@5�h@4��@4��@4��@4Z@41@3��@333@2�@2��@2�!@2-@1��@1�#@1��@1hs@1G�@1�@0Ĝ@0��@0�u@0bN@0Q�@0b@/�;@/��@/�@.��@.�@.�R@.��@.v�@.V@.E�@.@-��@-��@-p�@-?}@,�@,�/@,��@,��@,9X@+��@+��@+S�@+"�@*�@*��@*��@*�\@*M�@*M�@*=q@*-@)��@)�#@)��@)��@)�7@)G�@(Ĝ@(�@(r�@(bN@(  @'�P@'\)@';d@&�y@&��@&v�@&E�@&$�@%�@%�-@%��@%�h@%�@%�@%p�@%�@$�/@$�/@$��@$��@$Z@$�@#ƨ@#��@#t�@#S�@#"�@#@"�H@"��@"n�@"M�@!��@!��@!�^@!��@!��@!�7@!x�@!G�@!%@ ��@ Ĝ@ ��@ �u@ r�@ A�@   @�;@�@\)@
=@�y@�R@��@V@{@�-@�@`B@?}@V@��@��@�j@�D@z�@j@I�@�@�F@S�@C�@"�@��@�!@n�@-@�@�@��@�7@X@7L@%@�`@Ĝ@�@1'@ �@�@��@�@;d@�@��@�+@V@@��@�-@�h@p�@�@V@��@�@��@9X@1@�m@�
@�@dZ@33@�H@�!@�\@~�@n�@n�@^5@M�@�@��@�7@x�@X@&�@�@�@��@�`@��@��@A�@�w@�P@|�@;d@+@�y@�R@�R@��@v�@$�@�T@@�-@�@p�@`B@�@�j@z�@(�@��@�
@��@dZ@o@
�@
��@
�\@
M�@	��@	�^@	x�@	X@	G�@	�@��@�`@Ĝ@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�O�A�Q�A�S�A�O�A�M�A�Q�A�K�A�M�A�I�A�9XA�A��/AʸRAʙ�AʋDA�n�A�/A�%A��A��TA��
A��
A�ƨAɺ^AɬA�E�A�oA�
=A��A��`A���A�S�A��/Aũ�A�
=A��uA���A��`A�-A�E�A�S�A� �A���A�A��A��A�t�A�"�A���A�^5A�?}A�%A�n�A���A�$�A�hsA�ƨA�O�A�{A�n�A�t�A�l�A��A���A��`A�p�A�|�A�ƨA��7A�
=A�`BA�33A��HA�ĜA���A�-A��A�-A�hsA�ȴA���A��-A�r�A�-A���A��9A�K�A���A��jA�~�A�S�A�  A��PA���A��FA���A��A���A�z�A��yA�v�A�v�A��DA���A���A���A��;A�C�A��A�&�A�?}A�I�A�%A�z�A�jA��\A�;dA���A�7LAA~~�A|��Az�!Av��At��As&�ApbNAmK�Aj��Ah��Ag��Ae\)AcO�Aa�TA_�A[\)AY�AX{ATM�ARZAQƨAQ`BAP�AM��AK�;AJ��AI��AHbNAF��AFA�AE��AEx�AC�-A?��A=�
A<^5A;C�A:Q�A9VA8$�A7x�A6 �A57LA4ffA3O�A2�A1K�A/�wA.jA-�mA-\)A+�A)�A)S�A)�A(ĜA'ƨA&��A%��A$bNA#��A#%A"  A!hsA ��Ax�A�AbNAA��Av�A��A7LA^5A�A�+A|�A�AI�AXA��A �AVAQ�AA7LA��A�mAVA��A�^Al�A
�A
Q�A	x�A��A��A �A|�A�uA��AoAJAƨA��A��A��A��A b@���@�t�@�A�@�o@�X@�
=@�%@��@�!@�&�@��;@�^5@��`@�t�@��#@�K�@�7L@�Q�@�|�@���@�@���@�9X@��
@�33@��@ى7@���@�  @�"�@�^5@�?}@�9X@��y@щ7@���@ЋD@� �@��;@�"�@���@�&�@�I�@˥�@�"�@�J@��`@�j@�C�@�v�@�@���@�Ĝ@��@��y@°!@�ff@��@�O�@���@��;@���@��7@�1@��@��!@�{@�x�@�Q�@�@���@�V@�Q�@��@�t�@���@��h@��@��m@�S�@�$�@�`B@���@���@��@�J@�&�@���@�(�@��
@�
=@���@��#@�7L@�%@�(�@���@�|�@���@�\)@�ȴ@��@��@��@��m@��w@�ƨ@��P@�;d@�o@��@�\)@�S�@��y@�M�@�{@���@�`B@��@�r�@�l�@�33@�
=@���@�^5@�=q@�J@��@��^@��@�7L@�Ĝ@��@�r�@�ƨ@�;d@���@���@�v�@�=q@��@��-@�`B@��`@��D@�r�@�(�@���@��@���@�t�@�dZ@�33@��H@���@�^5@�-@�{@��^@��@�hs@�?}@�&�@�%@��@�Ĝ@���@��u@�z�@��;@�|�@�dZ@�@��R@��!@��\@�ff@�^5@�ff@�5?@���@���@���@��^@��7@�G�@��@�V@��@��j@��D@��@�z�@�b@�ƨ@�l�@�dZ@�S�@�S�@�K�@�"�@��@���@�n�@�M�@�5?@�$�@��@���@��T@���@���@��-@��h@�hs@�7L@�7L@�%@�%@���@�Ĝ@���@�9X@��@��F@���@��@�l�@�S�@�C�@�C�@�33@��y@���@���@���@�ff@�E�@�-@�{@���@���@�p�@�?}@�%@��/@���@�Q�@�b@�1@���@�+@���@��@��y@��y@���@���@�~�@�@��T@��^@���@���@���@�hs@�&�@���@���@�j@�w@�@~�y@~��@}�T@}p�@}?}@}V@|�j@|z�@{��@{��@{C�@{@z��@z��@z��@z�!@z^5@z-@y�@y�#@y�#@y��@y��@y��@y&�@x�9@xr�@w��@wl�@w;d@v�y@vȴ@v�+@vv�@vE�@u��@uO�@t�/@t�@tI�@t(�@s��@s�@s@r�@r��@r~�@r=q@q�7@p��@q&�@p�`@p�u@pA�@o|�@n��@n$�@m�@m�@m��@mp�@m?}@l��@lz�@lZ@l9X@k��@kC�@j��@jM�@i�^@iX@h��@hĜ@h��@h�u@hbN@g��@g�;@g;d@g
=@f�y@fE�@ep�@d��@d�@dz�@d9X@c�@cC�@c"�@b��@b~�@b-@a��@a��@aX@`�u@`b@_�@_\)@_�@^�y@^ȴ@^��@^V@^$�@^@]��@]�h@]/@\��@\�@\j@\I�@\�@[��@[�
@[ƨ@[��@[C�@[o@Z��@ZJ@Y�7@Y�@XQ�@W�P@Wl�@W;d@V�y@V��@V5?@V$�@U��@UO�@U/@T�/@T�D@T�@S��@Sƨ@St�@SS�@S33@R��@R^5@R�@RJ@Q�#@Qhs@QG�@Q&�@P��@PA�@O��@O\)@N��@N�R@NV@N$�@M�T@M�@MV@L�j@Lj@K�m@Kƨ@K��@K33@Ko@Ko@K@J��@J��@J�!@J�!@J~�@J-@I�^@I��@Ix�@I7L@H�`@HĜ@H��@HbN@H �@Hb@Gl�@G�@F��@G�@G;d@G
=@F��@E�@EO�@D��@D��@DI�@DI�@C��@Ct�@C"�@Co@B=q@A��@Ahs@A7L@@��@@bN@@b@?�;@?�w@?�P@?K�@?
=@?
=@?�@>��@>5?@=�@=��@=�@<��@<�@<��@<I�@<9X@<�@;�m@;��@;C�@:�H@:~�@:�@9�@9��@9X@9�@8��@8�u@8�@8Q�@8b@7�w@7�P@7;d@7�@6�@6��@6ff@5�@5@5@5�h@4��@4��@4��@4Z@41@3��@333@2�@2��@2�!@2-@1��@1�#@1��@1hs@1G�@1�@0Ĝ@0��@0�u@0bN@0Q�@0b@/�;@/��@/�@.��@.�@.�R@.��@.v�@.V@.E�@.@-��@-��@-p�@-?}@,�@,�/@,��@,��@,9X@+��@+��@+S�@+"�@*�@*��@*��@*�\@*M�@*M�@*=q@*-@)��@)�#@)��@)��@)�7@)G�@(Ĝ@(�@(r�@(bN@(  @'�P@'\)@';d@&�y@&��@&v�@&E�@&$�@%�@%�-@%��@%�h@%�@%�@%p�@%�@$�/@$�/@$��@$��@$Z@$�@#ƨ@#��@#t�@#S�@#"�@#@"�H@"��@"n�@"M�@!��@!��@!�^@!��@!��@!�7@!x�@!G�@!%@ ��@ Ĝ@ ��@ �u@ r�@ A�@   @�;@�@\)@
=@�y@�R@��@V@{@�-@�@`B@?}@V@��@��@�j@�D@z�@j@I�@�@�F@S�@C�@"�@��@�!@n�@-@�@�@��@�7@X@7L@%@�`@Ĝ@�@1'@ �@�@��@�@;d@�@��@�+@V@@��@�-@�h@p�@�@V@��@�@��@9X@1@�m@�
@�@dZ@33@�H@�!@�\@~�@n�@n�@^5@M�@�@��@�7@x�@X@&�@�@�@��@�`@��@��@A�@�w@�P@|�@;d@+@�y@�R@�R@��@v�@$�@�T@@�-@�@p�@`B@�@�j@z�@(�@��@�
@��@dZ@o@
�@
��@
�\@
M�@	��@	�^@	x�@	X@	G�@	�@��@�`@Ĝ@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBhBPBDB	7B
=BJBbBbBbBuB$�BXB_;BcTBbNBjB�JB��B��B}�B�B�B�B�B�Bq�Bn�Bk�B]/BO�BM�BR�BVB`BBcTBbNBe`BgmBdZBcTB`BBbNBe`Be`Be`BdZBaHB]/BZBR�BJ�BH�BC�BB�BF�BM�BP�B\)Be`BiyBdZBR�BJ�BC�B.B"�B�B�B�BhBDB��B��B�B�B�B�yB�NB�
BȴB�9B�B��B�%Bz�Bt�BdZBW
BG�B9XB�B1B
��B
�B
�/B
��B
�qB
�LB
�B
��B
�bB
z�B
]/B
L�B
G�B
=qB
0!B
�B
B	�B	�fB	��B	�qB	�B	��B	�oB	�B	v�B	iyB	ZB	>wB	33B	'�B	{B	B	B��B��B�B�/B��B��BȴBB�}B�jB�XB�9B�B��B��B��B��B�uB�VB�JB�=B�B�B}�By�Bu�Bp�Bk�Bl�Br�Bq�BjBgmBffBffBe`B`BB`BB\)B[#BYBW
BW
BXBW
BS�BO�BO�BM�BK�BI�BG�BF�BB�BC�BB�BB�B@�B=qB=qB;dB;dB:^B:^B9XB8RB8RB5?B2-B0!B-B-B)�B)�B'�B&�B&�B%�B%�B)�B'�B(�B&�B%�B$�B$�B!�B"�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B#�B#�B"�B"�B!�B#�B#�B$�B$�B%�B&�B(�B+B+B.B0!B0!B1'B1'B1'B33B5?B7LB8RB9XB:^B=qB>wB@�BA�BB�BC�BE�BF�BH�BJ�BJ�BK�BK�BM�BN�BP�BR�BS�BW
BXBXBZB]/B^5B`BB_;B^5BdZBl�Bo�Bq�Bs�Bt�Bw�Bx�Bx�Bz�By�B� B{�B~�B�%B�PB�\B�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�FB�qBĜB��B��B��B��B�B�)B�;B�BB�ZB�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	%B	
=B	VB	oB	{B	�B	�B	�B	�B	�B	!�B	$�B	)�B	,B	-B	.B	1'B	6FB	:^B	=qB	A�B	B�B	D�B	F�B	H�B	J�B	L�B	M�B	O�B	O�B	Q�B	T�B	VB	XB	]/B	aHB	cTB	ffB	hsB	iyB	jB	n�B	q�B	t�B	x�B	z�B	|�B	}�B	~�B	�B	�B	�%B	�%B	�+B	�7B	�DB	�JB	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�FB	�LB	�LB	�RB	�^B	�dB	�qB	��B	��B	B	B	ÖB	ĜB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�/B	�5B	�;B	�;B	�BB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
1B
1B
1B
+B
+B
1B
	7B
	7B
	7B
	7B
1B
1B
1B
1B
1B
	7B

=B
DB
DB
DB
JB
JB
PB
PB
VB
VB
PB
PB
VB
bB
hB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
#�B
#�B
$�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
&�B
&�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
,B
.B
.B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
0!B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
;dB
<jB
<jB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
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
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
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
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
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
bNB
bNB
bNB
bNB
bNB
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
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
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
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B{BuBhBPBDB	7B
=BJBbBbBbBuB$�BXB_;BcTBbNBjB�JB��B��B}�B�B�B�B�B�Bq�Bn�Bk�B]/BO�BM�BR�BVB`BBcTBbNBe`BgmBdZBcTB`BBbNBe`Be`Be`BdZBaHB]/BZBR�BJ�BH�BC�BB�BF�BM�BP�B\)Be`BiyBdZBR�BJ�BC�B.B"�B�B�B�BhBDB��B��B�B�B�B�yB�NB�
BȴB�9B�B��B�%Bz�Bt�BdZBW
BG�B9XB�B1B
��B
�B
�/B
��B
�qB
�LB
�B
��B
�bB
z�B
]/B
L�B
G�B
=qB
0!B
�B
B	�B	�fB	��B	�qB	�B	��B	�oB	�B	v�B	iyB	ZB	>wB	33B	'�B	{B	B	B��B��B�B�/B��B��BȴBB�}B�jB�XB�9B�B��B��B��B��B�uB�VB�JB�=B�B�B}�By�Bu�Bp�Bk�Bl�Br�Bq�BjBgmBffBffBe`B`BB`BB\)B[#BYBW
BW
BXBW
BS�BO�BO�BM�BK�BI�BG�BF�BB�BC�BB�BB�B@�B=qB=qB;dB;dB:^B:^B9XB8RB8RB5?B2-B0!B-B-B)�B)�B'�B&�B&�B%�B%�B)�B'�B(�B&�B%�B$�B$�B!�B"�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B#�B#�B"�B"�B!�B#�B#�B$�B$�B%�B&�B(�B+B+B.B0!B0!B1'B1'B1'B33B5?B7LB8RB9XB:^B=qB>wB@�BA�BB�BC�BE�BF�BH�BJ�BJ�BK�BK�BM�BN�BP�BR�BS�BW
BXBXBZB]/B^5B`BB_;B^5BdZBl�Bo�Bq�Bs�Bt�Bw�Bx�Bx�Bz�By�B� B{�B~�B�%B�PB�\B�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�3B�FB�qBĜB��B��B��B��B�B�)B�;B�BB�ZB�B�B�B�B��B��B��B��B��B��B��B��B	B	B	B	%B	
=B	VB	oB	{B	�B	�B	�B	�B	�B	!�B	$�B	)�B	,B	-B	.B	1'B	6FB	:^B	=qB	A�B	B�B	D�B	F�B	H�B	J�B	L�B	M�B	O�B	O�B	Q�B	T�B	VB	XB	]/B	aHB	cTB	ffB	hsB	iyB	jB	n�B	q�B	t�B	x�B	z�B	|�B	}�B	~�B	�B	�B	�%B	�%B	�+B	�7B	�DB	�JB	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�FB	�LB	�LB	�RB	�^B	�dB	�qB	��B	��B	B	B	ÖB	ĜB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�/B	�5B	�;B	�;B	�BB	�NB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
1B
1B
1B
+B
+B
1B
	7B
	7B
	7B
	7B
1B
1B
1B
1B
1B
	7B

=B
DB
DB
DB
JB
JB
PB
PB
VB
VB
PB
PB
VB
bB
hB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
#�B
#�B
$�B
#�B
#�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
&�B
&�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
,B
.B
.B
.B
.B
.B
.B
.B
.B
.B
/B
/B
/B
0!B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
;dB
<jB
<jB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
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
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
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
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
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
bNB
bNB
bNB
bNB
bNB
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
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
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
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
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
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210812100049                              AO  ARCAADJP                                                                    20210812100049    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210812100049  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210812100049  QCF$                G�O�G�O�G�O�0               