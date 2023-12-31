CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-07-12T00:51:41Z creation;2022-07-12T00:51:41Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220712005141  20220712010755  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               dA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @��[��l1   @��[T�>3@;�p��
=�c��l�C�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A!��A@  A`  A���A�  A�  A���A���A�  A�  A���B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B���B���B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C�C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Ck�fCn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C�  C��3C�  C��C�  C��3C��3C��C��C��C�  C��C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D&��D'� D(  D(� D(��D)y�D)��D*� D+  D+� D,fD,� D-  D-� D.fD.�fD/  D/� D0  D0� D1fD1�fD2  D2� D3  D3�fD4  D4� D5  D5� D6fD6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@fD@� DA  DA� DBfDB�fDC  DC� DD  DD� DD��DE� DF  DF� DG  DG� DH  DH� DH��DIy�DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DN��DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DU��DV� DW  DW� DX  DX� DY  DYy�DY��DZ� D[  D[y�D\  D\� D]  D]�fD^fD^� D_  D_y�D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dp��Dqy�Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}fD}� D~  D~� D  D� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�<�D�|�D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�<�DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D˼�D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D��3D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D��3D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D�|�D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�<�Dۀ D�� D�  D�@ D܃3D�� D���D�<�D݀ D�� D�  D�C3Dރ3D�� D���D�@ D߀ D�� D�  D�<�D�� D�� D�  D�C3D� D�� D�  D�@ D�|�D�� D�3D�@ D� D�� D�3D�@ D� D�� D�3D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D���D�  D�@ D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�=q@�=qA�RA=�A]�A~�RA��\A��\A�\)A�\)AΏ\Aޏ\A�\)A��\BG�BG�BG�BG�B&�HB/G�B7G�B?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B��
B��
B��
B��
B���B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�B�p�B�p�Bۣ�B�p�B�p�B��B��B��B��B���B���B��C��C��C��C��C	��C��C��C��C�RC��C��C��C��C��C�C��C!��C#��C%�RC'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg�RCi��Ck�RCm��Co��Cq��Cs��Cu��Cw��Cy�C{��C}��C��C���C��)C���C���C���C��)C��)C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C��)C��)C���C��)C���C���C��)C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{DnD�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{DnD�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D ��D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�D't{D'�{D(t{D(�D)nD)�D*t{D*�{D+t{D+��D,t{D,�{D-t{D-��D.z�D.�{D/t{D/�{D0t{D0��D1z�D1�{D2t{D2�{D3z�D3�{D4t{D4�{D5t{D5��D6t{D6�D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?��D@t{D@�{DAt{DA��DBz�DB�{DCt{DC�{DDt{DD�DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�DInDI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNnDN�DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�DVt{DV�{DWt{DW�{DXt{DX�{DYnDY�DZt{DZ�{D[nD[�{D\t{D\�{D]z�D]��D^t{D^�{D_nD_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{DpnDp�DqnDq�{Drt{Dr�{Dst{Ds�{Dtt{Dt�{Dut{Du�{Dvt{Dv�{Dwt{Dw�{Dxt{Dx�{Dyt{Dy�{Dzt{Dz�{D{t{D{�{D|t{D|��D}t{D}�{D~t{D~�{Dt{D��D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�}pD��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�=pD�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�w
D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�w
D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�=pD�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�=pD�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��
D��
D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��pD��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��
D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��pD�=pD�z=D��=D��=D�=pD�z=D��=D��=D�:=D�z=D��=D��=D�7
D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�7
D�z=D��=D��=D�7
D�w
D��=D��=D�:=D�w
D��
D��=D�:=D�z=D��=D��=D�:=D�z=Dº=D��=D�:=D�z=Dú=D��=D�7
D�z=Dĺ=D��=D�:=D�z=Dź=D��=D�:=D�z=Dƺ=D��=D�:=D�z=DǺ=D��=D�:=D�z=DȺ=D��=D�:=D�z=Dɺ=D��=D�:=D�z=Dʺ=D��=D�:=D�z=D˷
D��=D�:=D�z=D̺=D��=D�:=D�z=Dͺ=D��=D�:=D�z=Dκ=D��=D�:=D�z=DϺ=D��=D�:=D�z=Dк=D��=D�:=D�z=DѽpD��=D�:=D�z=DҺ=D��=D�:=D�z=DӺ=D��=D�:=D�z=DԺ=D��=D�:=D�z=DսpD��=D�:=D�z=Dֺ=D��=D�:=D�z=D׺=D��=D�:=D�w
Dغ=D��=D�:=D�z=Dٺ=D��=D�:=D�z=Dں=D��=D�7
D�z=Dۺ=D��=D�:=D�}pDܺ=D��
D�7
D�z=Dݺ=D��=D�=pD�}pD޺=D��
D�:=D�z=Dߺ=D��=D�7
D�z=D�=D��=D�=pD�z=D�=D��=D�:=D�w
D�=D��pD�:=D�z=D�=D��pD�:=D�z=D�=D��pD�=pD�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�7
D�z=D�=D��
D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D�=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�z=D��=D��=D�:=D�}pD��=D��=D�:=D�z=D��
D��=D�:=D�z=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A���A��A��>A���A���A��A���A��A��QA��cA��5A���A���A��/A��5A���AϒAΒ�A˪eA��AſHA��A�0�AÍ�A�'�A�A�D�A��KA�� A�T,A��^A��A��KA��DA��fA�y>A��>A���A���A���A��zA��4A��2A�g�A�@�A���A�aA��A�ZQA�Z�A��A�K^A��GA���A�r�A��A�S�A���A��A��A�4�A�2-A��A���A��A�w�A��A��qA��A�E�A���A��$A�.IA��mA�*�A��(A�5tA�xlA��wA�V�A�XA��LA�1[A��A��~A�O�A��UA���A�ǮA��BA�%zA���A��(A�1A��0A�/�A��A��cA���A�VmA��A�{�A���A��A��A�@�A�@A��A�A�A��A��A~�@A{HAw�At�As�.Arb�Aq��ApAny>Am��AkqAjv�Ah�}Ae��Ab�A^g8A[�AZ  AY7LAX9XAWAV	AUeAT(�AR��AQ��APXyAO�ANeAM1'AL��ALxlAL(�AK�ZAKAI1�AG˒AFs�AE�^AD��AC�AC_AB��AA�[A@�A@D�A>�WA=ZA<h�A;A:cA:E9A9��A9L0A8�A8zxA8A7��A74A6MA5�oA4��A5�A4͟A4V�A3�8A3��A3Q�A2K�A1GA0�A/�fA/�A-��A+A�A*�{A*�A)QA'��A'\�A&�zA&�A&uA%9XA$�A$ �A#FA"ZA!��A!!�A 0UA�	A2�As�A�$A�Am�A�)A�A0�A�tA�_Ar�A�>A�<A��A��AOA��A	A��A��A*�A��Av`A�#A�A
QA	p;A]�A�2A�UAn�A�@AD�A��A4�A8�A'RAA��A�A;A F�@��@���@�/@��]@��@��o@�ȴ@�C�@�*�@��@���@��h@�>�@�~�@�@�@�@�}@섶@��@��@�F�@�p�@��@�!�@�8@���@�Dg@��`@��@�Y@��@ټ@�iD@ة�@�/@Պ�@��@��@ѷ@�r�@�=�@�tT@��@̖�@��@��z@�iD@�6z@��@Ȝx@ǰ�@ś=@À4@�|�@��@���@��H@��M@�e�@��F@��@��@�=�@�(�@���@�q�@�@�Dg@���@��[@��@��@���@���@��@�$t@�1�@�IR@���@�'R@��@�e,@�{@��@�)�@���@�� @��W@��d@��@�� @���@�b�@���@��?@��m@��@���@�IR@���@��@�/@��@���@��@���@�a@�7L@�ں@�Ov@��@���@���@��~@��B@�h�@�Ov@�8�@�*�@��g@���@���@�L0@��@�9X@�@��@�Y@�Vm@�kQ@�V@�6@�zx@���@��@���@�"�@���@��m@��h@�h
@��@��@�Vm@�+k@�F�@�=�@�O�@�K�@�L�@��@�D�@�1�@��a@�*0@��}@�Z�@�M@���@���@��K@���@�y�@�J�@�<6@��@�w2@�Q@���@�e�@�� @�4@���@��F@���@���@�G�@��@��@��z@�Z�@�~@��)@��@��o@�ϫ@���@�}�@�}�@��7@���@��^@�@O@���@�W�@��K@��n@���@�33@��@�A @�L�@�Vm@�X�@�IR@�C�@�(�@���@��p@�y>@�"h@���@�G�@�$t@��c@�w�@�j@�r�@�`�@��@˒@�f@Z�@�F@y�@~�h@J#@dZ@j�@�@~��@~}V@l�@~$�@}?}@{ƨ@z{�@y[W@x�p@xXy@w�0@v�m@vH�@u�@u\�@t��@t�@t[�@t��@u�@tɆ@tr�@s��@s>�@s
=@r͟@r�x@r��@r�@q�@rM�@q}�@q	l@o��@o��@os@n��@n{�@n�@m�j@m��@m�=@m�^@m��@m}�@m�@m��@m[W@l�U@l~@k�@k�}@k��@k�0@k�@kE9@j�@j+k@j@i��@i��@i��@ip�@i*0@hV�@gy�@f� @f�@eO�@d��@d�4@d�Y@d�@c� @c�V@cl�@cH�@b�<@b� @bYK@b	@a�@a?}@`�9@_��@_��@_"�@^��@^��@]�@]�@]��@]X@]�@\ی@\�?@\�@\�O@\h�@\>B@\*�@\  @[�@[l�@[�@Z�@Z�R@Zh
@ZL0@Z�@Y�-@Y�@X�p@X�e@XI�@X�@W�+@W�$@V��@V��@V^5@VGE@V!�@Uԕ@Uw2@UVm@U?}@UF@Us�@U�3@U+�@TA�@S��@SA�@R��@R��@R��@RQ@Q@Q��@Q^�@P�$@Pl"@P]d@PC-@P�@O��@O�q@O�@Og�@O&@N��@N\�@N�b@N�@N}V@NJ�@N!�@M�@Mp�@M:�@M+�@M�@L�f@L�[@L��@L`�@L%�@L7@K�@K�q@KJ#@K�@J��@J�\@JTa@J$�@I��@I��@I�h@H�P@Hj@H<�@H"h@G�@G�6@GP�@G�@F�@F��@F($@E�)@E�z@Ea�@E@E@@E�@E@D��@D�j@D �@C��@C�@Cb�@C$t@B�@Bh
@B@�@A�@A!�@@�/@@��@@Q�@@@?�*@?dZ@?1�@?@>�y@>��@>{�@=��@=�-@=2a@<��@<h�@;�]@;ݘ@;n/@;@O@:�8@:��@:xl@:Ov@:�@9�@9�z@9��@9w2@94@8�E@8�@8H@7��@7�:@7&@6��@6�A@6kQ@6R�@5��@5�M@5%F@4�@4M@3��@3�@3t�@3_p@3C�@2�2@2�R@2Z�@2.�@1�T@1�~@1X@15�@0�	@0�@0j@/��@/�f@/��@/�{@/_p@/�@.ߤ@.�<@.a|@.Ov@.+k@.!�@-��@-8�@-;@,�z@,j@,:�@,�@+�@@+n/@+!-@+ i@*�@*�H@*�'@*��@*��@*��@*_�@*@�@*�@)��@)��@)��@)�^@)�@)m]@)@(��@(~@'��@'9�@&��@&xl@%��@%�d@%��@%�@%@%��@%@%�@%��@%��@%�^@%�@%^�@%�@$�@$��@$h�@$$@#|�@#+@#!-@#C@#@"�H@"��@"�1@"R�@"8�@"($@" �@!�@!�@ ��@ l"@ c�@ e�@ c�@ `�@ _@ [�@ ?�@ 	�@��@�@@RT@�@��@�@��@�+@:*@�@	@�T@�@��@��@5�@ی@�$@D�@�@��@��@_p@+@"�@�@Y@��@ȴ@�@{�@d�@GE@��@��@�@�@�@r�@_@A�@*�@�]@� @�a@��@�4@{J@s@F�@��@�c@ߤ@�s@҉@��@ȴ@��@�r@#:@��@ԕ@��@�@B�@֡@�e@�D@g8@7�@"h@خ@��@y�@Mj@&@�"@�,@��@l�@3�@��@�@}�@F@/@ \@��@ی@��@Ĝ@��@z�@oi@_@?�@˒@n/@��@ȴ@�L@i�@@��@�H@�@�7@�M@��@�M@��@��@�S@c�@5�@�@�5@�@�)@�9@��@j@K^@�@��@�;@�K@��@��@�@�$@��@y�@X�@>�@ i@
��@
�L@
��@
8�@	�j@	�=@	T�@	0�@	%F@	�@	+@	@	�@	;@	;@��@�e@�.@_@?�@�@  @�@��@j�@A�@��@҉@��@_�@J@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A���A��A��>A���A���A��A���A��A��QA��cA��5A���A���A��/A��5A���AϒAΒ�A˪eA��AſHA��A�0�AÍ�A�'�A�A�D�A��KA�� A�T,A��^A��A��KA��DA��fA�y>A��>A���A���A���A��zA��4A��2A�g�A�@�A���A�aA��A�ZQA�Z�A��A�K^A��GA���A�r�A��A�S�A���A��A��A�4�A�2-A��A���A��A�w�A��A��qA��A�E�A���A��$A�.IA��mA�*�A��(A�5tA�xlA��wA�V�A�XA��LA�1[A��A��~A�O�A��UA���A�ǮA��BA�%zA���A��(A�1A��0A�/�A��A��cA���A�VmA��A�{�A���A��A��A�@�A�@A��A�A�A��A��A~�@A{HAw�At�As�.Arb�Aq��ApAny>Am��AkqAjv�Ah�}Ae��Ab�A^g8A[�AZ  AY7LAX9XAWAV	AUeAT(�AR��AQ��APXyAO�ANeAM1'AL��ALxlAL(�AK�ZAKAI1�AG˒AFs�AE�^AD��AC�AC_AB��AA�[A@�A@D�A>�WA=ZA<h�A;A:cA:E9A9��A9L0A8�A8zxA8A7��A74A6MA5�oA4��A5�A4͟A4V�A3�8A3��A3Q�A2K�A1GA0�A/�fA/�A-��A+A�A*�{A*�A)QA'��A'\�A&�zA&�A&uA%9XA$�A$ �A#FA"ZA!��A!!�A 0UA�	A2�As�A�$A�Am�A�)A�A0�A�tA�_Ar�A�>A�<A��A��AOA��A	A��A��A*�A��Av`A�#A�A
QA	p;A]�A�2A�UAn�A�@AD�A��A4�A8�A'RAA��A�A;A F�@��@���@�/@��]@��@��o@�ȴ@�C�@�*�@��@���@��h@�>�@�~�@�@�@�@�}@섶@��@��@�F�@�p�@��@�!�@�8@���@�Dg@��`@��@�Y@��@ټ@�iD@ة�@�/@Պ�@��@��@ѷ@�r�@�=�@�tT@��@̖�@��@��z@�iD@�6z@��@Ȝx@ǰ�@ś=@À4@�|�@��@���@��H@��M@�e�@��F@��@��@�=�@�(�@���@�q�@�@�Dg@���@��[@��@��@���@���@��@�$t@�1�@�IR@���@�'R@��@�e,@�{@��@�)�@���@�� @��W@��d@��@�� @���@�b�@���@��?@��m@��@���@�IR@���@��@�/@��@���@��@���@�a@�7L@�ں@�Ov@��@���@���@��~@��B@�h�@�Ov@�8�@�*�@��g@���@���@�L0@��@�9X@�@��@�Y@�Vm@�kQ@�V@�6@�zx@���@��@���@�"�@���@��m@��h@�h
@��@��@�Vm@�+k@�F�@�=�@�O�@�K�@�L�@��@�D�@�1�@��a@�*0@��}@�Z�@�M@���@���@��K@���@�y�@�J�@�<6@��@�w2@�Q@���@�e�@�� @�4@���@��F@���@���@�G�@��@��@��z@�Z�@�~@��)@��@��o@�ϫ@���@�}�@�}�@��7@���@��^@�@O@���@�W�@��K@��n@���@�33@��@�A @�L�@�Vm@�X�@�IR@�C�@�(�@���@��p@�y>@�"h@���@�G�@�$t@��c@�w�@�j@�r�@�`�@��@˒@�f@Z�@�F@y�@~�h@J#@dZ@j�@�@~��@~}V@l�@~$�@}?}@{ƨ@z{�@y[W@x�p@xXy@w�0@v�m@vH�@u�@u\�@t��@t�@t[�@t��@u�@tɆ@tr�@s��@s>�@s
=@r͟@r�x@r��@r�@q�@rM�@q}�@q	l@o��@o��@os@n��@n{�@n�@m�j@m��@m�=@m�^@m��@m}�@m�@m��@m[W@l�U@l~@k�@k�}@k��@k�0@k�@kE9@j�@j+k@j@i��@i��@i��@ip�@i*0@hV�@gy�@f� @f�@eO�@d��@d�4@d�Y@d�@c� @c�V@cl�@cH�@b�<@b� @bYK@b	@a�@a?}@`�9@_��@_��@_"�@^��@^��@]�@]�@]��@]X@]�@\ی@\�?@\�@\�O@\h�@\>B@\*�@\  @[�@[l�@[�@Z�@Z�R@Zh
@ZL0@Z�@Y�-@Y�@X�p@X�e@XI�@X�@W�+@W�$@V��@V��@V^5@VGE@V!�@Uԕ@Uw2@UVm@U?}@UF@Us�@U�3@U+�@TA�@S��@SA�@R��@R��@R��@RQ@Q@Q��@Q^�@P�$@Pl"@P]d@PC-@P�@O��@O�q@O�@Og�@O&@N��@N\�@N�b@N�@N}V@NJ�@N!�@M�@Mp�@M:�@M+�@M�@L�f@L�[@L��@L`�@L%�@L7@K�@K�q@KJ#@K�@J��@J�\@JTa@J$�@I��@I��@I�h@H�P@Hj@H<�@H"h@G�@G�6@GP�@G�@F�@F��@F($@E�)@E�z@Ea�@E@E@@E�@E@D��@D�j@D �@C��@C�@Cb�@C$t@B�@Bh
@B@�@A�@A!�@@�/@@��@@Q�@@@?�*@?dZ@?1�@?@>�y@>��@>{�@=��@=�-@=2a@<��@<h�@;�]@;ݘ@;n/@;@O@:�8@:��@:xl@:Ov@:�@9�@9�z@9��@9w2@94@8�E@8�@8H@7��@7�:@7&@6��@6�A@6kQ@6R�@5��@5�M@5%F@4�@4M@3��@3�@3t�@3_p@3C�@2�2@2�R@2Z�@2.�@1�T@1�~@1X@15�@0�	@0�@0j@/��@/�f@/��@/�{@/_p@/�@.ߤ@.�<@.a|@.Ov@.+k@.!�@-��@-8�@-;@,�z@,j@,:�@,�@+�@@+n/@+!-@+ i@*�@*�H@*�'@*��@*��@*��@*_�@*@�@*�@)��@)��@)��@)�^@)�@)m]@)@(��@(~@'��@'9�@&��@&xl@%��@%�d@%��@%�@%@%��@%@%�@%��@%��@%�^@%�@%^�@%�@$�@$��@$h�@$$@#|�@#+@#!-@#C@#@"�H@"��@"�1@"R�@"8�@"($@" �@!�@!�@ ��@ l"@ c�@ e�@ c�@ `�@ _@ [�@ ?�@ 	�@��@�@@RT@�@��@�@��@�+@:*@�@	@�T@�@��@��@5�@ی@�$@D�@�@��@��@_p@+@"�@�@Y@��@ȴ@�@{�@d�@GE@��@��@�@�@�@r�@_@A�@*�@�]@� @�a@��@�4@{J@s@F�@��@�c@ߤ@�s@҉@��@ȴ@��@�r@#:@��@ԕ@��@�@B�@֡@�e@�D@g8@7�@"h@خ@��@y�@Mj@&@�"@�,@��@l�@3�@��@�@}�@F@/@ \@��@ی@��@Ĝ@��@z�@oi@_@?�@˒@n/@��@ȴ@�L@i�@@��@�H@�@�7@�M@��@�M@��@��@�S@c�@5�@�@�5@�@�)@�9@��@j@K^@�@��@�;@�K@��@��@�@�$@��@y�@X�@>�@ i@
��@
�L@
��@
8�@	�j@	�=@	T�@	0�@	%F@	�@	+@	@	�@	;@	;@��@�e@�.@_@?�@�@  @�@��@j�@A�@��@҉@��@_�@J@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BjBjB5BOBjBOB�BjB�B�B�B�B�B�BB�B�B�B�B�B��B�5B�gB�FBϑB��B�B�B�qB�WB�B��B�ZB��B��B�qB�=B��B�dBٴB��B�
B�9B�B�B�NB�BּB�1B��B OB��B��B߾B��B� B��B�-B�B��B�#B�4BvBiDBSuB@�B7�B.}B'�B#�B �BB[B�B�B��B�VB��B��B��B�B��Bq'Bd�B\�BW�BO�BB'B&�B[B	B��B�iB�=B�YB�.B�B��B�|B�VB��B��B��B|PBjeB[=BI�B?}B5ZB+QB(XB# B BB+B�B
�B
ΥB
�B
��B
�`B
��B
��B
��B
�BB
�YB
zDB
h�B
U�B
?�B
+B
#�B
�B
�B
�B
�B
�B
	RB
�B
 B	�*B	��B	�B	��B	�B	�$B	�B	�B	�B	�B	�B	�\B	ɆB	�zB	��B	��B	�rB	�3B	�B	��B	��B	�\B	�pB	�B	��B	�2B	�&B	� B	��B	�bB	�B	�=B	�B	��B	�B	�B	�B	��B	��B	�oB	B	.B	}<B	xRB	sMB	p;B	nIB	jKB	b�B	]dB	Z�B	ZB	Q�B	OBB	KDB	I�B	IB	D�B	CB	@OB	<�B	;B	8RB	6�B	4nB	1AB	/�B	.�B	+kB	*�B	$�B	"�B	!-B	 'B	�B	dB	�B		B	�B	�B	�B	hB	�B	�B	�B	�B	B		�B	1B	+B	B	�B��B�}B��B�B�"B��B��B��B��B��B��B�?B��B�B�B�B�B�B�UB�iB� B�B�wB�wB�qB��B�B�=B�=B�B��B�WB��B�WB�B�eB�B�B�eB��B��B��B��B�DB�B�yB�B�kB�=B�QB�6B��B��B�B��B��B�B��B��B��B�B�aB�GB��B�B�B�B��B��B��B�XB��B�B�	B�XB��B�DB��B�PB�B	B	B�HB��B	�B	mB	YB	
�B	B	�B	.B	<B	VB	B	.B	NB	B	�B	pB	[B	�B	 B	B	+B	1B	�B	$�B	&fB	%FB	%�B	'B	(�B	'8B	+B	0�B	2aB	4TB	7�B	8�B	;�B	AB	F�B	JrB	K�B	MB	O\B	R�B	VB	VB	VB	VmB	Z�B	\�B	\�B	]dB	]IB	^�B	b�B	c�B	d�B	jKB	m]B	n�B	oB	q�B	y�B	z�B	yrB	y>B	xlB	y	B	y�B	~wB	z�B	~�B	�9B	�EB	��B	�+B	��B	�B	�fB	�zB	��B	��B	��B	��B	�tB	�EB	�B	��B	�B	��B	�VB	�(B	�B	�B	��B	�,B	��B	��B	�/B	�ZB	��B	��B	�B	��B	��B	��B	�nB	�B	�B	��B	��B	�9B	�B	�xB	��B	��B	�=B	��B	̘B	�(B	ѝB	յB	ּB	ؓB	�WB	��B	�xB	�B	�dB	�pB	߾B	߾B	߾B	�4B	�B	�B	�>B	�DB	��B	�B	�cB	��B	��B	��B	��B	�%B	��B	��B	�B	��B	�B	�B	�B	��B	��B	��B	�cB
�B
�B
_B
xB
JB
�B
�B
�B
aB
�B
WB
xB
;B
�B
�B
qB
xB
VB
�B
B
�B
 vB
"B
$@B
%�B
)yB
-�B
-�B
-wB
-CB
,�B
-CB
./B
/B
/iB
1[B
5ZB
;�B
=�B
=�B
@B
@4B
@�B
A�B
A�B
C-B
DB
DMB
D�B
F�B
G�B
H�B
IRB
MjB
S�B
V�B
YB
Y�B
Y�B
ZB
ZB
ZB
ZkB
\xB
^5B
_VB
`\B
`�B
aB
aB
a�B
c�B
d&B
e,B
f�B
h�B
i�B
j�B
kB
mB
n/B
oB
o�B
pUB
shB
uB
w�B
x�B
y�B
zB
{�B
B
�B
�UB
��B
�-B
�B
��B
�B
�EB
�B
�rB
��B
�B
�DB
�JB
�B
�B
��B
��B
��B
�B
�B
��B
�aB
��B
�B
�B
�#B
��B
��B
��B
�B
�vB
��B
�:B
�:B
��B
�&B
��B
�B
�>B
��B
�_B
�KB
�kB
�B
�UB
�AB
��B
��B
�hB
�B
�hB
��B
�ZB
�B
��B
�LB
�LB
��B
��B
�RB
��B
��B
�JB
�B
��B
�jB
��B
�B
��B
ňB
�?B
ƨB
ǔB
ȴB
�RB
�lB
ɠB
��B
�#B
��B
�B
ˬB
ˬB
�B
̳B
�B
̈́B
�<B
��B
ϑB
�B
��B
�B
ѷB
��B
��B
�B
�aB
ՁB
��B
�?B
רB
��B
�EB
�1B
��B
�7B
�qB
�)B
�)B
�)B
�CB
�xB
��B
�5B
��B
�;B
�pB
�B
�B
�-B
�-B
�B
�nB
�&B
��B
�FB
�B
�B
�8B
�mB
�B
�XB
��B
�B
�B
��B
�eB
�B
�B
�qB
�B
�]B
�wB
��B
�/B
��B
�5B
�B
�oB
��B
��B
��B
�vB
��B
�aB
�hB
�9B
��B
��B
�FB
��B
��B
��B
�LB
��B
��B
��B
��B
�xB
��B
�B
�0B
�JB
�B
�B
��B
��B
��B
�(B
�]B
�wB
��B
�.B
��BB B B �B;B�BB[B�B�B-B-B�B�B�B�B�B%B?BB_B�B�BB1BfB�B�B�B	B	B	�B	�B	�B	�B
	B
	B
XB
�B
�B~B�BjBB�BBB�B�B�B�B�B�B�B�B�B�B�BbB�B�B�B�BBB�B�BoB�B�B�B@B�B,BFBFBFB,B�B{B�B�B�B�B�B�B�B�BgB9B
B�B�B
B
B$B�B�B�BB�B�B�BKB�B�B�BxB�B/B�B�B�B�BB5BjBjBjB�B�B�B�B�BB�B�BpB�B�B �B!�B!�B"B"hB"�B"�B#:B#�B#�B#�B$B$&B$&B$&B$&B$�B%FB%zB%�B%�B%�B&fB&�B'8B'RB'�B'�B'�B(sB(�B(�B)*B)DB)yB)�B*B*KB*�B*�B+B+�B+�B+�B,"B,qB,�B,�B,�B,�B-)B-)B-)B-]B-�B.IB/B/5B/OB/�B0UB0�B0�B0�B0�B0�B0�B1B0�B0�B0�B1AB1[B2B1�B1�B2B2-B2aB2�B2�B2�B33B3MB3hB3�B3�B3�B3�B3�B3�B4B4B4�B4�B4�B5B5�B5�B6`B6�B6�B6�B6�B7B72B72B72B7B7fB7�B7�B8B8B8�B8�B8�B9$B9XB9�B:*B:*B:DB:�B;JB;dB;444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   BjBjB5BOBjBOB�BjB�B�B�B�B�B�BB�B�B�B�B�B��B�5B�gB�FBϑB��B�B�B�qB�WB�B��B�ZB��B��B�qB�=B��B�dBٴB��B�
B�9B�B�B�NB�BּB�1B��B OB��B��B߾B��B� B��B�-B�B��B�#B�4BvBiDBSuB@�B7�B.}B'�B#�B �BB[B�B�B��B�VB��B��B��B�B��Bq'Bd�B\�BW�BO�BB'B&�B[B	B��B�iB�=B�YB�.B�B��B�|B�VB��B��B��B|PBjeB[=BI�B?}B5ZB+QB(XB# B BB+B�B
�B
ΥB
�B
��B
�`B
��B
��B
��B
�BB
�YB
zDB
h�B
U�B
?�B
+B
#�B
�B
�B
�B
�B
�B
	RB
�B
 B	�*B	��B	�B	��B	�B	�$B	�B	�B	�B	�B	�B	�\B	ɆB	�zB	��B	��B	�rB	�3B	�B	��B	��B	�\B	�pB	�B	��B	�2B	�&B	� B	��B	�bB	�B	�=B	�B	��B	�B	�B	�B	��B	��B	�oB	B	.B	}<B	xRB	sMB	p;B	nIB	jKB	b�B	]dB	Z�B	ZB	Q�B	OBB	KDB	I�B	IB	D�B	CB	@OB	<�B	;B	8RB	6�B	4nB	1AB	/�B	.�B	+kB	*�B	$�B	"�B	!-B	 'B	�B	dB	�B		B	�B	�B	�B	hB	�B	�B	�B	�B	B		�B	1B	+B	B	�B��B�}B��B�B�"B��B��B��B��B��B��B�?B��B�B�B�B�B�B�UB�iB� B�B�wB�wB�qB��B�B�=B�=B�B��B�WB��B�WB�B�eB�B�B�eB��B��B��B��B�DB�B�yB�B�kB�=B�QB�6B��B��B�B��B��B�B��B��B��B�B�aB�GB��B�B�B�B��B��B��B�XB��B�B�	B�XB��B�DB��B�PB�B	B	B�HB��B	�B	mB	YB	
�B	B	�B	.B	<B	VB	B	.B	NB	B	�B	pB	[B	�B	 B	B	+B	1B	�B	$�B	&fB	%FB	%�B	'B	(�B	'8B	+B	0�B	2aB	4TB	7�B	8�B	;�B	AB	F�B	JrB	K�B	MB	O\B	R�B	VB	VB	VB	VmB	Z�B	\�B	\�B	]dB	]IB	^�B	b�B	c�B	d�B	jKB	m]B	n�B	oB	q�B	y�B	z�B	yrB	y>B	xlB	y	B	y�B	~wB	z�B	~�B	�9B	�EB	��B	�+B	��B	�B	�fB	�zB	��B	��B	��B	��B	�tB	�EB	�B	��B	�B	��B	�VB	�(B	�B	�B	��B	�,B	��B	��B	�/B	�ZB	��B	��B	�B	��B	��B	��B	�nB	�B	�B	��B	��B	�9B	�B	�xB	��B	��B	�=B	��B	̘B	�(B	ѝB	յB	ּB	ؓB	�WB	��B	�xB	�B	�dB	�pB	߾B	߾B	߾B	�4B	�B	�B	�>B	�DB	��B	�B	�cB	��B	��B	��B	��B	�%B	��B	��B	�B	��B	�B	�B	�B	��B	��B	��B	�cB
�B
�B
_B
xB
JB
�B
�B
�B
aB
�B
WB
xB
;B
�B
�B
qB
xB
VB
�B
B
�B
 vB
"B
$@B
%�B
)yB
-�B
-�B
-wB
-CB
,�B
-CB
./B
/B
/iB
1[B
5ZB
;�B
=�B
=�B
@B
@4B
@�B
A�B
A�B
C-B
DB
DMB
D�B
F�B
G�B
H�B
IRB
MjB
S�B
V�B
YB
Y�B
Y�B
ZB
ZB
ZB
ZkB
\xB
^5B
_VB
`\B
`�B
aB
aB
a�B
c�B
d&B
e,B
f�B
h�B
i�B
j�B
kB
mB
n/B
oB
o�B
pUB
shB
uB
w�B
x�B
y�B
zB
{�B
B
�B
�UB
��B
�-B
�B
��B
�B
�EB
�B
�rB
��B
�B
�DB
�JB
�B
�B
��B
��B
��B
�B
�B
��B
�aB
��B
�B
�B
�#B
��B
��B
��B
�B
�vB
��B
�:B
�:B
��B
�&B
��B
�B
�>B
��B
�_B
�KB
�kB
�B
�UB
�AB
��B
��B
�hB
�B
�hB
��B
�ZB
�B
��B
�LB
�LB
��B
��B
�RB
��B
��B
�JB
�B
��B
�jB
��B
�B
��B
ňB
�?B
ƨB
ǔB
ȴB
�RB
�lB
ɠB
��B
�#B
��B
�B
ˬB
ˬB
�B
̳B
�B
̈́B
�<B
��B
ϑB
�B
��B
�B
ѷB
��B
��B
�B
�aB
ՁB
��B
�?B
רB
��B
�EB
�1B
��B
�7B
�qB
�)B
�)B
�)B
�CB
�xB
��B
�5B
��B
�;B
�pB
�B
�B
�-B
�-B
�B
�nB
�&B
��B
�FB
�B
�B
�8B
�mB
�B
�XB
��B
�B
�B
��B
�eB
�B
�B
�qB
�B
�]B
�wB
��B
�/B
��B
�5B
�B
�oB
��B
��B
��B
�vB
��B
�aB
�hB
�9B
��B
��B
�FB
��B
��B
��B
�LB
��B
��B
��B
��B
�xB
��B
�B
�0B
�JB
�B
�B
��B
��B
��B
�(B
�]B
�wB
��B
�.B
��BB B B �B;B�BB[B�B�B-B-B�B�B�B�B�B%B?BB_B�B�BB1BfB�B�B�B	B	B	�B	�B	�B	�B
	B
	B
XB
�B
�B~B�BjBB�BBB�B�B�B�B�B�B�B�B�B�B�BbB�B�B�B�BBB�B�BoB�B�B�B@B�B,BFBFBFB,B�B{B�B�B�B�B�B�B�B�BgB9B
B�B�B
B
B$B�B�B�BB�B�B�BKB�B�B�BxB�B/B�B�B�B�BB5BjBjBjB�B�B�B�B�BB�B�BpB�B�B �B!�B!�B"B"hB"�B"�B#:B#�B#�B#�B$B$&B$&B$&B$&B$�B%FB%zB%�B%�B%�B&fB&�B'8B'RB'�B'�B'�B(sB(�B(�B)*B)DB)yB)�B*B*KB*�B*�B+B+�B+�B+�B,"B,qB,�B,�B,�B,�B-)B-)B-)B-]B-�B.IB/B/5B/OB/�B0UB0�B0�B0�B0�B0�B0�B1B0�B0�B0�B1AB1[B2B1�B1�B2B2-B2aB2�B2�B2�B33B3MB3hB3�B3�B3�B3�B3�B3�B4B4B4�B4�B4�B5B5�B5�B6`B6�B6�B6�B6�B7B72B72B72B7B7fB7�B7�B8B8B8�B8�B8�B9$B9XB9�B:*B:*B:DB:�B;JB;dB;444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220712005002  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220712005141  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220712005141  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220712005141                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220712095146  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220712095146  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20220712010755                      G�O�G�O�G�O�                