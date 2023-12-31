CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:23Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170923  20220204114423  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               |A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @���i�1   @����^�@6�E�����b�n��O�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    |A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�33B�33B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D�!�D�b=D��D��RD��D�Q�D��
D���D�%�D�\)D��{D���D��D�K�Dڧ\D�ؤD�D�R=D�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�=q@�=qA�A=�A]�A}�A��\A��\A��\A��\AΏ\Aޏ\A�\A�\(BG�BG�BG�BG�B'G�B/G�B7G�B?G�BGG�BOG�BW�B_G�BgG�BoG�BwG�BG�B��
B��
B��
B���B�p�B�p�B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�Bף�B��
Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��)C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D��Dt{D�{Dt{D�{Dt{D��Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/t{D/�{D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI��DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{DV�{DWt{DW�{DXt{DX�{DYt{DY�{DZt{DZ�{D[t{D[�{D\t{D\�{D]t{D]�{D^t{D^�{D_t{D_�{D`t{D`�{Dat{Da�{Dbt{Db�{Dct{Dc�{Ddt{Dd�{Det{De�{Dft{Df�{Dgt{Dg�{Dht{Dh�{Dit{Di�{Djt{Dj�{Dkt{Dk�{Dlt{Dl�{Dmt{Dm�{Dnt{Dn�{Dot{Do�{Dpt{Dp�{Dqt{Dq�{Drt{Dr�{Dst{Ds�{Dtt{Dt��Dy�)D�)D�\zD��QD��D��D�L)D��GD��3D�  D�VfD���D��D�)D�FDڡ�D���D�QD�LzD�pD��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AȍPAș�Aȡ�Aȡ�Aȣ�Aȣ�Aȣ�Aȣ�Aȣ�Aȥ�Aȧ�Aȧ�Aȩ�AȬAȬAȬAȮAȰ!AȰ!AȬAȬAȮAȮAȰ!AȮAȮAȮAȮAȴ9AȮAȩ�Aȴ9AȶFAȴ9Aȴ9AȬAȴ9AȶFAȶFAȶFAȲ-AȸRAȼjAȼjAȾwAȲ-Aȡ�A�G�A��mA���A�O�A��^A���A��A�jA��A���A��A�5?A���A���A�hsA�Q�A�A�A���A��
A�(�A�I�A�z�A��9A�\)A���A��#A���A��7A��^A�&�A��TA���A�x�A��9A�ȴA�r�A��9A��\A�ĜA��7A��A�ƨA�I�A�r�A�{A��#A��PA��HA�?}A���A�VA���A��A�n�A�^5A�{A��/A�x�A�K�A�9XA���A�G�A�t�A�K�A�ĜA�ƨA;dA}&�A{��Az�AvbAr^5Aq�Ao��Am��Ak��Aj��Aj=qAh�uAfE�Adn�Abz�A_�A^$�A\��A\JAZ�`AXȴAT-ARĜAQ�AO�AM�TAKXAG�#AG�AE;dABQ�AAA@�A>A�A<$�A:jA9��A9O�A8v�A7x�A6�yA5�A41'A2�jA2{A0VA.ĜA-��A-%A,n�A*��A(jA'�#A'�-A'&�A&~�A%�A$M�A"��A"$�A!�A!oA 5?A\)A&�A�/A�A�#A�HA(�A��AJA��A�!AZA��A�wA1'AO�A�AA��AA�AK�Ar�A��A��A��AƨAoAE�A;dA  A�7A/A�A
�DA	l�A�jA��A�A�A�wA��A5?A;dA ��A VA -@�(�@��@�ȴ@��;@�ff@�O�@� �@�ƨ@�;d@�$�@���@�5?@��-@�7L@�@�w@�~�@�/@��m@�ȴ@�h@��;@���@���@�I�@�1@�^5@�V@�&�@ܼj@��`@�ƨ@�33@ج@�O�@�A�@���@�5?@��@ёh@�z�@�V@͙�@̛�@���@��@���@ǶF@���@�x�@��m@���@��-@�ƨ@�33@��@��@��D@�I�@�1'@���@�ƨ@�\)@��@���@��@�ff@��^@��@�/@���@��u@�J@��+@��R@��@�G�@��@�(�@�\)@�ȴ@�M�@��@���@���@���@��7@�V@�r�@�  @�ƨ@��^@���@��
@�C�@��@�"�@�-@�p�@��/@�G�@�r�@�33@��!@���@�^5@�+@��@�{@���@�@���@��9@�9X@���@�\)@�;d@��\@�=q@���@�/@�r�@�A�@��@��m@��F@���@�;d@��y@���@���@���@���@�V@�{@���@�@�hs@���@���@���@���@�(�@�9X@�Z@�b@���@�(�@�I�@���@���@�@�C�@��@�~�@�M�@�hs@�1@��H@�V@��@�33@��\@�V@�J@���@��-@�O�@�7L@�/@�%@���@���@��D@���@���@��9@� �@���@� �@��@��F@�S�@�33@�C�@�;d@�"�@�
=@��@��y@��H@��H@�ȴ@�ȴ@���@��@��-@���@�x�@�?}@�/@�&�@���@���@��@��u@�j@�9X@�(�@�(�@�(�@��@��@�1@��;@���@�ƨ@���@�t�@�\)@�33@�ȴ@���@���@�M�@�V@�{@���@�x�@�V@��D@�Z@��@�t�@�C�@�"�@�"�@�o@�"�@�@��\@�E�@�=q@�=q@�=q@��@�O�@�/@�%@��`@���@�j@�A�@�9X@�1'@�1@���@��P@�;d@�"�@�
=@���@���@�n�@�M�@�M�@�5?@w�@p�@l|�@c�@]�@Vں@O@O@IDg@D2�@;Mj@4PH@.�@(�@%o @ ��@�z@��@�6@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AȍPAș�Aȡ�Aȡ�Aȣ�Aȣ�Aȣ�Aȣ�Aȣ�Aȥ�Aȧ�Aȧ�Aȩ�AȬAȬAȬAȮAȰ!AȰ!AȬAȬAȮAȮAȰ!AȮAȮAȮAȮAȴ9AȮAȩ�Aȴ9AȶFAȴ9Aȴ9AȬAȴ9AȶFAȶFAȶFAȲ-AȸRAȼjAȼjAȾwAȲ-Aȡ�A�G�A��mA���A�O�A��^A���A��A�jA��A���A��A�5?A���A���A�hsA�Q�A�A�A���A��
A�(�A�I�A�z�A��9A�\)A���A��#A���A��7A��^A�&�A��TA���A�x�A��9A�ȴA�r�A��9A��\A�ĜA��7A��A�ƨA�I�A�r�A�{A��#A��PA��HA�?}A���A�VA���A��A�n�A�^5A�{A��/A�x�A�K�A�9XA���A�G�A�t�A�K�A�ĜA�ƨA;dA}&�A{��Az�AvbAr^5Aq�Ao��Am��Ak��Aj��Aj=qAh�uAfE�Adn�Abz�A_�A^$�A\��A\JAZ�`AXȴAT-ARĜAQ�AO�AM�TAKXAG�#AG�AE;dABQ�AAA@�A>A�A<$�A:jA9��A9O�A8v�A7x�A6�yA5�A41'A2�jA2{A0VA.ĜA-��A-%A,n�A*��A(jA'�#A'�-A'&�A&~�A%�A$M�A"��A"$�A!�A!oA 5?A\)A&�A�/A�A�#A�HA(�A��AJA��A�!AZA��A�wA1'AO�A�AA��AA�AK�Ar�A��A��A��AƨAoAE�A;dA  A�7A/A�A
�DA	l�A�jA��A�A�A�wA��A5?A;dA ��A VA -@�(�@��@�ȴ@��;@�ff@�O�@� �@�ƨ@�;d@�$�@���@�5?@��-@�7L@�@�w@�~�@�/@��m@�ȴ@�h@��;@���@���@�I�@�1@�^5@�V@�&�@ܼj@��`@�ƨ@�33@ج@�O�@�A�@���@�5?@��@ёh@�z�@�V@͙�@̛�@���@��@���@ǶF@���@�x�@��m@���@��-@�ƨ@�33@��@��@��D@�I�@�1'@���@�ƨ@�\)@��@���@��@�ff@��^@��@�/@���@��u@�J@��+@��R@��@�G�@��@�(�@�\)@�ȴ@�M�@��@���@���@���@��7@�V@�r�@�  @�ƨ@��^@���@��
@�C�@��@�"�@�-@�p�@��/@�G�@�r�@�33@��!@���@�^5@�+@��@�{@���@�@���@��9@�9X@���@�\)@�;d@��\@�=q@���@�/@�r�@�A�@��@��m@��F@���@�;d@��y@���@���@���@���@�V@�{@���@�@�hs@���@���@���@���@�(�@�9X@�Z@�b@���@�(�@�I�@���@���@�@�C�@��@�~�@�M�@�hs@�1@��H@�V@��@�33@��\@�V@�J@���@��-@�O�@�7L@�/@�%@���@���@��D@���@���@��9@� �@���@� �@��@��F@�S�@�33@�C�@�;d@�"�@�
=@��@��y@��H@��H@�ȴ@�ȴ@���@��@��-@���@�x�@�?}@�/@�&�@���@���@��@��u@�j@�9X@�(�@�(�@�(�@��@��@�1@��;@���@�ƨ@���@�t�@�\)@�33@�ȴ@���@���@�M�@�V@�{@���@�x�@�V@��D@�Z@��@�t�@�C�@�"�@�"�@�o@�"�@�@��\@�E�@�=q@�=q@�=q@��@�O�@�/@�%@��`@���@�j@�A�@�9X@�1'@�1@���@��P@�;d@�"�@�
=@���@���@�n�@�M�G�O�@�5?@w�@p�@l|�@c�@]�@Vں@O@O@IDg@D2�@;Mj@4PH@.�@(�@%o @ ��@�z@��@�6@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�#B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�
B
�B
�B
�B
�
B
�
B
�
B
�
B
�B
�B
�B
�)B
�;B
�TB�B,B:^BO�BS�BM�Bn�By�Bu�Br�B�B��B�DBo�Bu�B�DB�?BĜB�LB�RB�B�XB��B�)B�`B��B  B��B��B��B��B��B��B��B��B��BB��B��B�B�TB��B��B�9B�hB{�Bv�B7LB
�B
�ZB
�fB
��B
B
�'B
�B
�B
�9B
ĜB
�qB
�?B
�!B
�B
�B
��B
�B
t�B
XB
@�B
�B
PB
B	��B	�/B	�^B	�B	��B	��B	�VB	�%B	�B	v�B	iyB	[#B	P�B	@�B	7LB	0!B	+B	!�B	�B	B��B�B�BB�
B��B�XB�3B�B��B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�=B�%B�B�B~�B{�Bw�Bv�Bu�Bs�Bp�Bn�Bl�BhsBffBhsBhsBe`BcTBcTB`BBYBXBXB\)BhsB��B�qBŢBȴB��B��B��B�)B�;B�B��B��B��B��B��B��BɺBĜB��B�dB�3B�B��B��B�+B~�Bz�Bn�Be`B`BBYBR�BN�BH�BE�BD�BA�B@�B9XB7LB8RB33B2-B1'B/B0!B1'B33B5?B49B49B49B49B33B49B6FB7LB9XB;dB;dB<jB>wBD�BE�BL�BL�BQ�B]/BaHBdZBbNB^5B\)BaHB^5B`BBgmBjBjBr�Bs�Bs�Bq�Bo�Bo�Bm�Br�Bu�Bt�Bu�Br�Bq�Bs�B|�B�B�+B�7B�PB�VB��B��B��B��B�B�-B�LB�LB�}BÖB�qBĜB��B��B��B��B��B��B��B��B�B�NB�sB�B�B�B�B�B�B�B�mB�mB�sB�B��B��B��B��B��B��B��B��B��B	B	DB	
=B	
=B	PB	VB	VB	JB	PB	PB	\B	hB	uB	oB	uB	�B	�B	�B	�B	�B	 �B	"�B	&�B	(�B	(�B	+B	.B	1'B	49B	6FB	9XB	:^B	;dB	=qB	?}B	A�B	D�B	I�B	P�B	R�B	S�B	S�B	YB	_;B	bNB	cTB	cTB	iyB	jB	l�B	m�B	n�B	k�B	ffB	aHB	]/B	`BB	aHB	cTB	ffB	gmB	iyB	n�B	n�B	n�B	o�B	q�B	u�B	y�B	z�B	{�B	|�B	|�B	� B	�B	�B	�+B	�7B	�DB	�PB	�VB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�?B	�FB	�LB	�RB	�XB	�XB	�^B	�dB	�jB	�jB	�}B	��B	ÖB	ĜB	ÖB	ĜB	ŢB	ŢB	ƨB	ŢB	ŢB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�;B	�BB	�BB	�HB	�HB	�TB	�TB	�ZB	�ZB	�ZB	�]B	�jB
B
�B
�B
#�B
1�B
9rB
D�B
I�B
QNB
V�B
X�B
Y�B
]�B
_pB
f2B
j�B
p�B
r�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
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
��B
��B
��B
�B
�)BpB�B.+BC�BG�BA�BbcBm�Bi�Bf{Bt�B�IBBckBi�BB�B�bB�B�B��B� BǾB��B�%B�B��B�B�B�B�B�B�B�B�B�B��B�B�B�eB�B��B£B�B�9Bo�Bj�B+&B
�B
�=B
�IB
��B
�vB
�B
��B
��B
�!B
��B
�YB
�(B
�
B
��B
��B
�nB
wB
h�B
LB
4yB
�B
LB	�B	��B	�0B	�cB	�!B	��B	��B	�`B	z0B	uB	j�B	]�B	O3B	D�B	4�B	+`B	$6B	B	�B	�B�,B��B�B�`B�)B��B�{B�WB�9B�B�@B�(B�B��B��B��B��B�B�B�B��B��B��B��B��B~iBzQBw?Bu3Bs'BpBk�Bj�Bi�Bg�Bd�Bb�B`�B\�BZ�B\�B\�BY�BW�BW�BTtBMJBLDBLDBP]B\�B�B��B��B��B�B��B��B�QB�cB�?B�'B�B�B��B��B��B��B��B��B��B�`B�<B�B��B{]Bs-BoBb�BY�BTyBMOBG+BCB<�B9�B8�B5�B4�B-�B+�B,�B'pB&jB%dB#YB$^B%dB'pB)}B(wB(wB(wB(wB'qB(wB*�B+�B-�B/�B/�B0�B2�B8�B9�BA
BA
BF)BQkBU�BX�BV�BRqBPfBU�BRrBT~B[�B^�B^�Bf�Bg�Bg�Be�Bc�Bc�Ba�Bf�Bi�Bh�Bi�Bf�Be�Bg�Bq*ByZB{fB}rB��B��B��B��B�B�4B�LB�dB��B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�EBւBܦB߸B��B��B��B��B��B޳BۡBۡBܧB��B��B�B�B�B�B�B�B�B�,B�8B�uB�nB�oB	�B	�B	�B	 {B	�B	�B	�B	�B	�B	�B	�B		�B	�B	�B	�B	�B	�B	B	B	%B	%B	1B	"CB	%VB	(gB	*tB	-�B	.�B	/�B	1�B	3�B	5�B	8�B	=�B	EB	GB	H$B	H$B	MBB	SfB	VyB	WB	WB	]�B	^�B	`�B	a�B	b�B	_�B	Z�B	UsB	Q[B	TnB	UtB	WB	Z�B	[�B	]�B	b�B	b�B	b�B	c�B	e�B	i�B	nB	oB	pB	qB	qB	t)B	xBB	yHB	{SB	}_B	lB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	�(B	�3B	�@B	�LB	�dB	�kB	�pB	�vB	�|B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�&B	�2B	�9B	�9B	�EB	�EB	�EB	�EB	�KB	�]B	�dB	�dB	�jB	�jB	�uB	�uB	�{B	�{G�O�B	�~B	��B	�0B
�B
�B
�B
%�B
-�B
8�B
>
B
EiB
J�B
MB
NB
Q�B
S�B
ZLB
^�B
eB
gB
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.18 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.012(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144232022020411442320220204114423  AO  ARCAADJP                                                                    20200619170923    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170923  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170923  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114423  IP                  G�O�G�O�G�O�                