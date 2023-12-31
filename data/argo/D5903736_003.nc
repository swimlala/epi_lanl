CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:07Z AOML 3.0 creation; 2016-05-31T19:14:24Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230507  20160531121425  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_003                   2C  D   APEX                            5368                            041511                          846 @�9-�@1   @�9-�M� @3w�O�;d�d��O�;d1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@���A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy��D�3D�FfD�� D��fD��D�FfD���D�ٚD�	�D�<�D��3D�ɚD�3D�@ Dڀ D��D�3D�33D�s3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@�\)@��\AG�A>�HA]G�A}G�A���A���A���A���AΣ�Aޣ�A��A���BQ�BQ�BQ�BQ�B'Q�B/Q�B7Q�B?Q�BGQ�BOQ�BWQ�B_Q�BgQ�BoQ�BwQ�BQ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bè�BǨ�B˨�BϨ�BӨ�Bר�Bۨ�Bߨ�B��B��B��B��B��B���B���B���C�{C�{C�{C�C	�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C!�{C#�C%�{C'�{C)�{C+�{C-�{C/�{C1�{C3�{C5�{C7�{C9�{C;�{C=�{C?�{CA�{CC�{CE�{CG�{CI�{CK�{CM�{CO�{CQ�{CS�{CU�{CW�{CY�{C[�{C]�{C_�{Ca�{Cc�{Ce�{Cg�{Ci�{Ck�{Cm�{Co�{Cq�{Cs�{Cu�{Cw�{Cy�{C{�{C}�{C�{C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=C��=D uD �DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D	uD	�D
uD
�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�DuD�D uD �D!uD!�D"uD"�D#uD#�D$uD$�D%uD%�D&uD&�D'uD'�D(uD(�D)uD)�D*uD*�D+uD+�D,uD,�D-uD-�D.uD.�D/uD/�D0uD0�D1uD1�D2uD2�D3uD3�D4uD4�D5uD5�D6uD6�D7uD7�D8uD8�D9uD9�D:uD:�D;uD;�D<uD<�D=uD=�D>uD>�D?uD?�D@uD@�DAuDA�DBuDB�DCuDC�DDuDD�DEuDE�DFuDF�DGuDG�DHuDH�DIuDI�DJuDJ�DKuDK�DLuDL�DMuDM�DNuDN�DOuDO�DPuDP�DQuDQ�DRuDR�DSuDS�DTuDT�DUuDU�DVuDV�DWuDW�DXuDX�DYuDY�DZuDZ�D[uD[�D\uD\�D]uD]�D^uD^�D_uD_�D`uD`�DauDa�DbuDb�DcuDc�DduDd�DeuDe�DfuDf�DguDg�DhuDh�DiuDi�DjuDj�DkuDk�DluDl�DmuDm�DnuDn�DouDo�DpuDp�DquDq�DruDr�DsuDs�Dt[�Dy��D��D�@�D���D���D�)D�@�D��)D��)D�)D�7\D�}�D��)D��D�:�D�z�D�\D��D�-�D�m�D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�~�AȍPAȍPAȑhAȓuAȕ�Aȕ�Aȗ�Aș�Aș�Aș�Aȗ�Aȗ�Aȗ�Aȗ�Aș�Aȟ�Aȡ�Aȣ�Aȣ�Aȧ�Aȩ�Aȡ�Aȣ�Aȡ�Aȟ�Aȟ�Aț�Aȝ�Aȟ�Aȟ�Aȗ�AȓuAȍPAȃA�C�A�%A���A�v�A�bA�l�A��Aŏ\A�?}A��A��;A�"�A��AþwAöFAìAã�AÑhA�r�A�hsA�^5A�M�A�I�A�I�A�A�A�+A��A�;dA�O�A���A��PA�=qA���A�;dA�VA��TA�5?A��A��hA�{A�hsA�%A���A��HA���A�p�A�O�A��RA�/A��A���A��TA��PA�(�A��
A���A�M�A���A�{A���A�33A��jA�M�A��!A��A��hA��yA���A�oA��hA�ƨA�I�A��9A�-A� �A�A~$�A{oAx��AwXAt�Ar��Aq�mAp�yApbNAo��AodZAn  Al�Al�Ah�jAe�Ac\)Ab�DA`bNA[�AW�PATĜAQ�AL�AKhsAJ�!AIdZAHĜAG�AFI�AE�PAE+AD  AB$�AA��AA%A?K�A=dZA<bNA;�TA;�A;/A:��A:�A8�DA7"�A6A4v�A3��A2-A1��A1�7A0��A0-A/&�A.�uA.5?A-&�A+t�A*�DA(��A'G�A&ffA%�A%�A$5?A �A �!A I�AƨAA;dA^5A�A�\A��A��A�/A��A�AE�A|�A\)AbNA9XA�\A��A
=A
A�A	"�A�Av�A1AhsAA�yA�Az�A��Al�A%A��A1'A�A?}A �A $�@��P@�@�Q�@�K�@���@�n�@���@�D@���@���@��@�?}@���@��;@��@�M�@���@�+@��@�X@�O�@�O�@��@�hs@�  @�t�@�l�@�dZ@�;d@�@���@�R@�O�@�9@�Q�@�9X@�  @�
=@��@�K�@�v�@�M�@�V@�n�@�M�@܋D@��T@��@�bN@�l�@�o@���@֏\@��@Ցh@�(�@�@�`B@�&�@Ь@�Z@���@���@�@͉7@���@�r�@˶F@ʏ\@ǝ�@Å@�v�@���@�/@��j@�A�@��y@�-@��T@��@��@���@�E�@�=q@��@�V@���@�z�@��m@�"�@���@��m@��@�  @��
@��
@�l�@�dZ@��R@�J@��-@�x�@��/@�Z@�9X@�1@�
=@���@�5?@��7@���@�9X@�1'@�1@���@��+@�{@��@���@���@��@���@��h@�x�@�G�@��9@� �@��
@���@��P@�l�@�o@�n�@���@��-@���@���@�p�@�O�@�Ĝ@�~�@�5?@��@�J@�@�hs@�`B@���@�V@��9@��@��D@�z�@�z�@�bN@�1'@��m@��@�;d@��@���@��H@��!@�ff@�{@��@�x�@���@�z�@�I�@�A�@�b@���@��m@���@���@�dZ@�K�@�33@��R@�v�@�^5@��@���@��^@��-@���@�x�@�X@�?}@�V@��@��j@���@��@�Q�@��
@��P@�l�@�K�@�"�@��!@�=q@��@���@���@�O�@�%@��`@��/@��u@�9X@�b@���@��@���@�9X@�O�@�&�@���@�A�@��@�S�@��@��R@�{@���@���@�Ĝ@��j@��@�Z@��@��w@��@�+@��H@���@�E�@�{@��@��-@�hs@�X@�7L@��/@��u@�A�@��@���@�ƨ@�l�@�;d@�+@��@�ȴ@�ȴ@��R@�V@�v�@���@��H@��H@��!@�~�@�@���@��/@���@�I�@�hs@�j@wl�@p��@h�`@_K�@W+@PA�@JJ@B�H@;�m@3�F@-�@(bN@"=q@$�@��@S�@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�~�AȍPAȍPAȑhAȓuAȕ�Aȕ�Aȗ�Aș�Aș�Aș�Aȗ�Aȗ�Aȗ�Aȗ�Aș�Aȟ�Aȡ�Aȣ�Aȣ�Aȧ�Aȩ�Aȡ�Aȣ�Aȡ�Aȟ�Aȟ�Aț�Aȝ�Aȟ�Aȟ�Aȗ�AȓuAȍPAȃA�C�A�%A���A�v�A�bA�l�A��Aŏ\A�?}A��A��;A�"�A��AþwAöFAìAã�AÑhA�r�A�hsA�^5A�M�A�I�A�I�A�A�A�+A��A�;dA�O�A���A��PA�=qA���A�;dA�VA��TA�5?A��A��hA�{A�hsA�%A���A��HA���A�p�A�O�A��RA�/A��A���A��TA��PA�(�A��
A���A�M�A���A�{A���A�33A��jA�M�A��!A��A��hA��yA���A�oA��hA�ƨA�I�A��9A�-A� �A�A~$�A{oAx��AwXAt�Ar��Aq�mAp�yApbNAo��AodZAn  Al�Al�Ah�jAe�Ac\)Ab�DA`bNA[�AW�PATĜAQ�AL�AKhsAJ�!AIdZAHĜAG�AFI�AE�PAE+AD  AB$�AA��AA%A?K�A=dZA<bNA;�TA;�A;/A:��A:�A8�DA7"�A6A4v�A3��A2-A1��A1�7A0��A0-A/&�A.�uA.5?A-&�A+t�A*�DA(��A'G�A&ffA%�A%�A$5?A �A �!A I�AƨAA;dA^5A�A�\A��A��A�/A��A�AE�A|�A\)AbNA9XA�\A��A
=A
A�A	"�A�Av�A1AhsAA�yA�Az�A��Al�A%A��A1'A�A?}A �A $�@��P@�@�Q�@�K�@���@�n�@���@�D@���@���@��@�?}@���@��;@��@�M�@���@�+@��@�X@�O�@�O�@��@�hs@�  @�t�@�l�@�dZ@�;d@�@���@�R@�O�@�9@�Q�@�9X@�  @�
=@��@�K�@�v�@�M�@�V@�n�@�M�@܋D@��T@��@�bN@�l�@�o@���@֏\@��@Ցh@�(�@�@�`B@�&�@Ь@�Z@���@���@�@͉7@���@�r�@˶F@ʏ\@ǝ�@Å@�v�@���@�/@��j@�A�@��y@�-@��T@��@��@���@�E�@�=q@��@�V@���@�z�@��m@�"�@���@��m@��@�  @��
@��
@�l�@�dZ@��R@�J@��-@�x�@��/@�Z@�9X@�1@�
=@���@�5?@��7@���@�9X@�1'@�1@���@��+@�{@��@���@���@��@���@��h@�x�@�G�@��9@� �@��
@���@��P@�l�@�o@�n�@���@��-@���@���@�p�@�O�@�Ĝ@�~�@�5?@��@�J@�@�hs@�`B@���@�V@��9@��@��D@�z�@�z�@�bN@�1'@��m@��@�;d@��@���@��H@��!@�ff@�{@��@�x�@���@�z�@�I�@�A�@�b@���@��m@���@���@�dZ@�K�@�33@��R@�v�@�^5@��@���@��^@��-@���@�x�@�X@�?}@�V@��@��j@���@��@�Q�@��
@��P@�l�@�K�@�"�@��!@�=q@��@���@���@�O�@�%@��`@��/@��u@�9X@�b@���@��@���@�9X@�O�@�&�@���@�A�@��@�S�@��@��R@�{@���@���@�Ĝ@��j@��@�Z@��@��w@��@�+@��H@���@�E�@�{@��@��-@�hs@�X@�7L@��/@��u@�A�@��@���@�ƨ@�l�@�;d@�+@��@�ȴ@�ȴ@��R@�V@�v�@���@��H@��H@��!@�~�@�@���@��/@���@�I�@�hs@�j@wl�@p��@h�`@_K�@W+@PA�@JJ@B�H@;�m@3�F@-�@(bN@"=q@$�@��@S�@��@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Br�Br�Br�Br�Br�Bs�Bs�Bt�Bu�Bv�Bx�Bw�B}�B�bB��B��B��B��B��B��B��B��B�B�'B��B�B�NB�B�B�B{B�B%�B7LB[#Bw�B{�B~�B�B�B�B�B�B�B�B�B�B�+B�+B�%B�Bu�BhsB^5BQ�B@�B0!B$�B\B�BĜB�RB�3B��B��B�JBt�BhsBYBI�B:^B2-B'�BB�HB�B��B��B��B��BȴBǮBĜB�jB��B�+Bp�B<jB,B�B	7B
�B
��B
�LB
��B
�{B
�=B
t�B
e`B
`BB
R�B
=qB
,B
 �B
hB
B	��B	��B	�B	�B	�B	�HB	�B	��B	�qB	��B	��B	�{B	�B	hsB	S�B	D�B	33B	!�B	$�B	$�B	%�B	$�B	"�B	�B	 �B	�B	�B	�B	{B	hB	
=B	B	B	  B��B��B��B��B�B�B�sB�TB�;B�)B�B�B�
B��B��B��B��B��BŢB��B�qB�^B�RB�FB�9B�!B��B��B��B��B��B��B�hB�VB�JB�7B�%B�B}�B{�By�Bv�Br�Bp�Bn�Bl�BjBhsBgmBe`BdZBcTBbNBbNBaHBaHBaHB`BB_;B_;B_;B_;B_;B^5B^5B^5B\)BZBYBXBVBS�BS�BVBcTBjBq�Bu�Bv�Bv�Bu�Bs�Br�Bn�Bl�Br�Bs�Bu�Bw�B� B�B�B�%B�+B�+B�%B�+B�+B�%B�1B�=B�JB�JB�PB�JB�+B�PB�\B�bB�bB�bB�oB��B��B��B��B��B��B��B��B��B��B��B�B�'B�9B�FB�LB�LB�LB�LB�FB�?B�3B�?B�?B�!B�'B�'B�-B�9B�9B�3B�LBÖBȴB��B��B��B��B�B�NB�mB�yB�B�B�B�B�yB�B�B�B�B�B�B��B��B	B	B	B	B	+B	
=B	bB	oB	�B	�B	%�B	)�B	-B	0!B	1'B	6FB	:^B	=qB	@�B	A�B	B�B	C�B	F�B	F�B	H�B	M�B	S�B	S�B	VB	ZB	[#B	^5B	aHB	dZB	e`B	e`B	e`B	e`B	e`B	gmB	m�B	p�B	r�B	t�B	v�B	w�B	z�B	~�B	~�B	� B	�B	�B	�+B	�+B	�7B	�DB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�9B	�?B	�FB	�RB	�XB	�^B	�^B	�dB	�jB	�}B	��B	B	ÖB	ƨB	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�TB	�ZB	�TB	�TB	�TB	�`B	�ZB	�ZB	�ZB	�ZB	�fB	�fB	�mB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
  B	��B	��B	��B
  B
1B
PB
�B
!�B
(�B
2-B
8RB
>wB
C�B
H�B
N�B
VB
[#B
_;B
e`B
iyB
n�B
r�B
v�B
z�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Br�Br�Br�Br�Br�Bs�Bs�Bt�Bu�Bv�Bx�Bw�B~B�sB��B��B��B��B��B��B��B��B�B�5B��B�.B�_B��B�B�B�B�B%�B7]B[8Bw�B{�BB�B�%B�%B�!B�B� B�%B�%B�2B�<B�@B�:B�Bu�Bh�B^GBRB@�B05B$�BpB�BĭB�bB�CB�B��B�]Bt�Bh�BY%BI�B:lB2;B( B"B�WB�"B�B��B��B��B��BǽBĭB�zB��B�:Bp�B<|B,B�B	LB
�B
��B
�aB
�	B
��B
�QB
t�B
evB
`YB
S
B
=�B
,"B
 �B
�B
6B	�B	��B	��B	�B	�B	�aB	�1B	� B	��B	�B	��B	��B	�6B	h�B	TB	D�B	3UB	!�B	$�B	$�B	&B	$�B	"�B	�B	 �B	�B	�B	�B	�B	�B	
aB	CB	*B	 %B�B�B��B��B��B�B�B�xB�bB�OB�DB�<B�2B�B�B�B��B��B��B��B��B��B�yB�lB�bB�KB�B�B�B��B��B��B��B�~B�sB�aB�NB�2B~B|BzBv�Br�Bp�Bn�Bl�Bj�Bh�Bg�Be�Bd�BcBbzBbxBarBatBauB`nB_hB_fB_eB_gB_hB^aB^aB^`B\SBZGBYBBX=BV/BT%BT%BV0Bc~Bj�Bq�Bu�Bv�Bv�Bu�Bs�Br�Bn�Bl�Br�Bs�Bu�Bw�B�,B�CB�BB�OB�WB�WB�OB�WB�VB�MB�]B�gB�sB�sB�zB�tB�VB�|B��B��B��B��B��B��B��B��B��B� B�B�B�B�B�B�B�*B�OB�`B�kB�uB�uB�vB�tB�mB�iB�[B�hB�jB�HB�NB�PB�TB�_B�_B�ZB�uBþB��B��B��B��B�B�<B�uB�B�B�B�B��B��B�B�B�B��B��B��B��B��B�B	4B	4B	6B	>B	QB	
`B	�B	�B	�B	�B	&B	*B	-/B	0CB	1HB	6fB	:�B	=�B	@�B	A�B	B�B	C�B	F�B	F�B	H�B	M�B	TB	TB	V%B	Z?B	[EB	^VB	ajB	dzB	e�B	e�B	e�B	e�B	e�B	g�B	m�B	p�B	r�B	t�B	v�B	w�B	z�B	B	B	�$B	�2B	�AB	�KB	�KB	�WB	�eB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�(B	�3B	�@B	�HB	�MB	�SB	�YB	�\B	�cB	�rB	�wB	�{B	�|B	��B	��B	��B	��B	¬B	ôB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�(B	�;B	�qB	�vB	�sB	�sB	�qB	�B	�wB	�xB	�vB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
 B
 B
"B
#B
#B
 B	�B	�B	�B
 B
PB
mB
�B
!�B
)B
2IB
8nB
>�B
C�B
H�B
N�B
VB
[>B
_TB
e~B
i�B
n�B
r�B
v�B
z�B
}11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.17 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214252016053112142520160531121425  AO  ARCAADJP                                                                    20140721230507    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230507  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230507  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121425  IP                  G�O�G�O�G�O�                