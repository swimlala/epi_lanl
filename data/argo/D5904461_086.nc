CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-29T20:17:00Z AOML 3.0 creation; 2016-08-07T21:36:41Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151129201700  20160807143641  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               VA   AO  5286_8897_086                   2C  D   APEX                            6531                            072314                          846 @ׂf���V1   @ׂg[��@3\�hr��c!�-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    VA   B   B   @�33@�  A   A   A@  Aa��A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B���B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3Dy��D��D�33D���D�� D�fD�<�D�vfD��fD�fD�6fD��3D��3D�3D�@ Dڌ�D�ٚD� D�L�D�3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @~�R@�(�@�(�A{A>{A_�A~{A�
=A�
=A��
A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B���B�\)B�\)B��\B�B���B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HCǮC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi��Ck�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD��DxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtK�Dy�D��D�/\D���D��)D��D�8�D�r�D�D��D�2�D�\Dǿ\D�\D�<)Dڈ�D���D�)D�H�D�\D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�x�A�z�A�x�A�z�A�|�A�|�A�z�A�z�A�|�A�~�AԁAԃAԃAԅAԅAԅAԅAԅAԅAԇ+Aԉ7AԋDAԋDAԍPAԏ\Aԏ\AԓuAԓuAԓuAԓuAԕ�Aԕ�Aԗ�Aԗ�Aԙ�Aԙ�Aԙ�Aԙ�Aԕ�AԓuAԓuAӇ+A��HA���AΛ�A̋DA�I�A��A�  A��A�9XA�oA��TA�$�A���A�JA�ffA���A���A�G�A��A���A��A�"�A�;dA���A��HA��A�v�A�  A�5?A�z�A��A��A�~�A���A�|�A�E�A��^A�VA� �A�1A��DA��`A��TA�ZA�  A�&�A���A��A�ȴA���A��A���A��A�%A���A�oA�
=A��A��A��A�S�A��PA�^5A��hA�=qA��uA�ZA�n�A~�HA{S�Az�+Ay"�AvffAr�`Ao�mAl�yAk�Ai�wAhM�Ae\)AbJA`M�A_S�A]S�AY��AU�#ASS�AOƨALjAK7LAI%AG�AF~�AD�+AA��A@�+A>�A;��A9��A8^5A6�A5`BA4�A4��A3t�A2��A1?}A/�mA/�-A//A.��A.(�A-l�A,�RA+t�A*�+A)�wA)A'�;A&�`A&Q�A$ĜA#��A"�A!��A!XA!oA �\A��A�AE�A��A��A��AA�A��AȴA��A��A�\AhsA�\A��A�A�+A��A��A�A  A?}A�RAƨA
�A	�mA�RAĜA	S�A	C�A��A(�A-A�A��A�!A��AC�A��A(�A�A%A�HA�HA�A�TA ��@��F@�-@��j@�r�@�
=@�
=@��@�M�@��@���@�~�@�V@�F@���@�;d@���@�R@�$�@�+@�-@�/@�b@���@�\)@�@��@��@� �@�(�@�o@���@��u@ߥ�@�@�x�@�`B@��@���@��;@��y@�{@��#@�x�@�G�@�(�@��
@��
@֧�@Լj@�v�@�p�@�p�@��/@�
=@̃@�1@�&�@�%@���@ˍP@��@�x�@ȋD@Ǯ@�"�@Ɵ�@�hs@�
=@��@�@���@��w@�(�@�1'@�Z@��@�C�@�K�@��H@�`B@��9@��@�(�@�o@�%@��m@���@���@���@�@�X@���@��@�A�@�X@��j@��@���@�dZ@�K�@���@��@�K�@�
=@���@��-@��^@��9@�
=@���@�hs@�O�@�O�@�%@��@�X@�hs@��j@�1'@�Q�@�r�@��@���@�bN@��@�I�@�33@�p�@���@�n�@���@�33@��@�ff@�%@�A�@���@��`@���@�J@��+@���@�;d@���@�1@�A�@�z�@��`@�7L@��j@�(�@�1@��
@��w@��y@�^5@�{@��@�?}@��@�9X@��@�r�@��@���@�ƨ@���@�^5@�@��@�n�@�5?@���@��@�p�@�%@��u@�r�@�A�@�j@�j@� �@��w@�|�@�@��!@���@�5?@�@��7@���@�5?@���@�p�@�Ĝ@��j@��@��@��`@��/@���@��D@��m@��F@��m@��u@��9@��9@��9@�z�@��@��m@���@�ƨ@��P@�S�@���@��!@�ff@�`B@�z�@�z�@�S�@���@���@�"�@�l�@�G�@�ƨ@�t�@�;d@�
=@���@�;d@��;@��u@��@���@�Ĝ@�z�@�9X@��@� �@��F@��@��@���@�;d@��!@�{@���@�O�@�/@��j@� �@��F@��@�\)@�
=@��!@���@�n�@�V@�5?@�5?@�=q@�E�@�^5@�V@�5?@�@�^5@{o@u`B@kƨ@`��@X�9@Q�@J^5@C�@=��@7�w@2=q@,�@(b@!hs@��@5?@hs@�h@	��@$�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�x�A�z�A�x�A�z�A�|�A�|�A�z�A�z�A�|�A�~�AԁAԃAԃAԅAԅAԅAԅAԅAԅAԇ+Aԉ7AԋDAԋDAԍPAԏ\Aԏ\AԓuAԓuAԓuAԓuAԕ�Aԕ�Aԗ�Aԗ�Aԙ�Aԙ�Aԙ�Aԙ�Aԕ�AԓuAԓuAӇ+A��HA���AΛ�A̋DA�I�A��A�  A��A�9XA�oA��TA�$�A���A�JA�ffA���A���A�G�A��A���A��A�"�A�;dA���A��HA��A�v�A�  A�5?A�z�A��A��A�~�A���A�|�A�E�A��^A�VA� �A�1A��DA��`A��TA�ZA�  A�&�A���A��A�ȴA���A��A���A��A�%A���A�oA�
=A��A��A��A�S�A��PA�^5A��hA�=qA��uA�ZA�n�A~�HA{S�Az�+Ay"�AvffAr�`Ao�mAl�yAk�Ai�wAhM�Ae\)AbJA`M�A_S�A]S�AY��AU�#ASS�AOƨALjAK7LAI%AG�AF~�AD�+AA��A@�+A>�A;��A9��A8^5A6�A5`BA4�A4��A3t�A2��A1?}A/�mA/�-A//A.��A.(�A-l�A,�RA+t�A*�+A)�wA)A'�;A&�`A&Q�A$ĜA#��A"�A!��A!XA!oA �\A��A�AE�A��A��A��AA�A��AȴA��A��A�\AhsA�\A��A�A�+A��A��A�A  A?}A�RAƨA
�A	�mA�RAĜA	S�A	C�A��A(�A-A�A��A�!A��AC�A��A(�A�A%A�HA�HA�A�TA ��@��F@�-@��j@�r�@�
=@�
=@��@�M�@��@���@�~�@�V@�F@���@�;d@���@�R@�$�@�+@�-@�/@�b@���@�\)@�@��@��@� �@�(�@�o@���@��u@ߥ�@�@�x�@�`B@��@���@��;@��y@�{@��#@�x�@�G�@�(�@��
@��
@֧�@Լj@�v�@�p�@�p�@��/@�
=@̃@�1@�&�@�%@���@ˍP@��@�x�@ȋD@Ǯ@�"�@Ɵ�@�hs@�
=@��@�@���@��w@�(�@�1'@�Z@��@�C�@�K�@��H@�`B@��9@��@�(�@�o@�%@��m@���@���@���@�@�X@���@��@�A�@�X@��j@��@���@�dZ@�K�@���@��@�K�@�
=@���@��-@��^@��9@�
=@���@�hs@�O�@�O�@�%@��@�X@�hs@��j@�1'@�Q�@�r�@��@���@�bN@��@�I�@�33@�p�@���@�n�@���@�33@��@�ff@�%@�A�@���@��`@���@�J@��+@���@�;d@���@�1@�A�@�z�@��`@�7L@��j@�(�@�1@��
@��w@��y@�^5@�{@��@�?}@��@�9X@��@�r�@��@���@�ƨ@���@�^5@�@��@�n�@�5?@���@��@�p�@�%@��u@�r�@�A�@�j@�j@� �@��w@�|�@�@��!@���@�5?@�@��7@���@�5?@���@�p�@�Ĝ@��j@��@��@��`@��/@���@��D@��m@��F@��m@��u@��9@��9@��9@�z�@��@��m@���@�ƨ@��P@�S�@���@��!@�ff@�`B@�z�@�z�@�S�@���@���@�"�@�l�@�G�@�ƨ@�t�@�;d@�
=@���@�;d@��;@��u@��@���@�Ĝ@�z�@�9X@��@� �@��F@��@��@���@�;d@��!@�{@���@�O�@�/@��j@� �@��F@��@�\)@�
=@��!@���@�n�@�V@�5?@�5?@�=q@�E�@�^5@�V@�5?G�O�@�^5@{o@u`B@kƨ@`��@X�9@Q�@J^5@C�@=��@7�w@2=q@,�@(b@!hs@��@5?@hs@�h@	��@$�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
�B
�B�BǮB�BBH�BffBjB�1B�B�-B�}B��B��B�FB��B�9B�XB�qBBȴB��B�;B�B�HB�B��B��B��BŢB�qB�9B�B��B�oB�%B� Bs�BffB`BBT�BK�BE�B7LBuB+B��B�;BŢB�B��Bv�BdZBVB;dB�B
��B
�B
�#B
��B
�RB
�3B
��B
�oB
}�B
p�B
\)B
O�B
=qB
.B
bB	��B	�B	�)B	B	��B	��B	�B	}�B	q�B	gmB	R�B	@�B	9XB	2-B	$�B	bB��B�B�;B��BɺBB�qB�RB�-B�B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B�'B�dB�dBBĜBŢBĜBŢBĜBƨBĜBÖB�}B�dB�^B�wBBÖBĜBƨBŢBɺB��B��B��B��B��B��B��B�
B�B��B��B�#B�BB�mB�sB�TB�)B�#B�B�B��B��B��B�B�yB�B�B�B�B��B	B	  B��B��B��B��B	B	  B	B	+B	
=B	1B	%B	B��B��B�B�B�B��B��B��B��B�B�B�B�NB�NB�ZB�fB	JB	�B	�B	�B	uB	JB	B	+B	DB	JB	\B	VB	oB	�B	{B	�B	�B	�B	�B	�B	 �B	!�B	!�B	%�B	&�B	'�B	'�B	(�B	/B	1'B	1'B	-B	-B	-B	49B	;dB	:^B	6FB	8RB	A�B	E�B	A�B	@�B	?}B	<jB	:^B	8RB	6FB	49B	/B	)�B	33B	0!B	+B	+B	2-B	7LB	8RB	7LB	7LB	7LB	5?B	49B	49B	0!B	=qB	ZB	jB	ffB	e`B	k�B	o�B	q�B	t�B	s�B	q�B	dZB	[#B	[#B	\)B	^5B	`BB	`BB	cTB	dZB	cTB	cTB	cTB	bNB	hsB	iyB	ffB	e`B	e`B	gmB	gmB	iyB	m�B	t�B	u�B	t�B	v�B	}�B	�B	�1B	�7B	�JB	�PB	�JB	�7B	�B	� B	z�B	u�B	z�B	�B	~�B	|�B	}�B	�B	�bB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�?B	�FB	�FB	�FB	�FB	�LB	�LB	�LB	�LB	�XB	�dB	�jB	�wB	ÖB	ȴB	ɺB	ŢB	ÖB	ÖB	ÖB	ŢB	ǮB	ǮB	ƨB	ȴB	ǮB	ǮB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	ɺB	ɺB	��B	ɺB	ȴB	ǮB	ŢB	�wB	�qB	�wB	��B	��B	ĜB	ƨB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�/B	�)B	�5B	�5B	�5B	�;B	�BB	�HB	�NB	�BB	�5B	�;B	�5B	�)B	�5B	�NB	�ZB	�)B	�
B	�B	�B	�B	�#B	�HB	�mB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
1B
%B
DB
�B
"�B
%�B
-B
6FB
=qB
B�B
F�B
O�B
T�B
W
B
\)B
`BB
gmB
k�B
p�B
s�B
x�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
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
�	B
�B
�B�BǹB�'B*BH�BfqBj�B�<B�B�;B��B��B��B�PB��B�EB�dB�~BB��B��B�HB�B�PB�B��B��B��BűB�|B�GB�B��B�{B�2B�Bs�BftB`MBU
BK�BE�B7WBB0B�B�FBūB�"B��Bv�BdeBVB;rB�B
��B
�B
�1B
��B
�eB
�AB
�
B
�B
~B
p�B
\;B
O�B
=�B
.'B
vB	��B	�B	�@B	£B	�B	��B	�7B	~B	q�B	g�B	SB	@�B	9pB	2IB	$�B	�B�	B�B�[B��B��B°B��B�vB�PB�+B�B��B�
B�B�B�%B�+B�)B�B�B�B�
B�B�B�B�IB��B��B±BľB��BľB��BĿB��B��BùB��B��B�B��B°BöBľB��B��B��B��B��B��B�B�B�!B�B�-B�%B�B�B�CB�bB�B�B�uB�GB�@B�>B�$B�B�B�B�@B�B�B�B�B��B�B	%B	 B�B��B��B��B	%B	  B	*B	IB	
YB	PB	CB	$B�B��B�B��B��B��B��B��B��B��B�B�B�nB�oB�zB�B	gB	�B	�B	�B	�B	hB	,B	IB	aB	gB	yB	uB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	!�B	&B	'B	(B	(B	)B	/8B	1DB	1DB	-)B	-+B	-*B	4UB	;�B	:yB	6bB	8nB	A�B	E�B	A�B	@�B	?�B	<�B	:{B	8pB	6bB	4WB	/8B	*B	3QB	0@B	+B	+B	2JB	7fB	8kB	7gB	7gB	7gB	5[B	4VB	4TB	0@B	=�B	Z9B	j�B	f�B	ezB	k�B	o�B	q�B	t�B	s�B	q�B	duB	[=B	[>B	\EB	^QB	`\B	`]B	cqB	duB	cnB	cpB	cmB	biB	h�B	i�B	f�B	e|B	e{B	g�B	g�B	i�B	m�B	t�B	u�B	t�B	v�B	~B	�7B	�LB	�PB	�bB	�jB	�eB	�PB	�1B	�B	z�B	u�B	z�B	�B	B	}B	~B	�4B	�}B	��B	��B	��B	��B	��B	�B	�B	�"B	�-B	�@B	�PB	�VB	�^B	�aB	�]B	�^B	�eB	�cB	�fB	�bB	�oB	�B	��B	��B	ëB	��B	��B	ŻB	ëB	ðB	ìB	ŻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	źB	��B	��B	��B	��B	��B	ĵB	��B	ŹB	��B	��B	��B	��B	��B	��B	�B	�B	�,B	�;B	�AB	�FB	�?B	�OB	�NB	�NB	�QB	�]B	�_B	�eB	�YB	�MB	�TB	�NB	�AB	�LB	�fB	�oB	�?B	�B	�B	�B	�%B	�9B	�^B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B
 B
 B
 B
B
#B
)B
3G�O�B
:B
ZB
�B
"�B
%�B
-%B
6\B
=�B
B�B
F�B
O�B
UB
WB
\=B
`UB
g�B
k�B
p�B
s�B
x�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436412016080714364120160807143641  AO  ARCAADJP                                                                    20151129201700    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151129201700  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151129201700  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143641  IP                  G�O�G�O�G�O�                