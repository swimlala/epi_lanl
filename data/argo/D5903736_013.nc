CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:13Z AOML 3.0 creation; 2016-05-31T19:14:26Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230513  20160531121426  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_013                   2C  D   APEX                            5368                            041511                          846 @�R�H/�1   @�R���/�@3��x����d*��+1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bq��Bw��B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DM��DNy�DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D�3D�FfD��3D��fD�fD�9�D��fD���D���D�9�D��3D��3D���D�@ D�vfD���D�3D�<�D�i�D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @~�R@�(�@�(�A{A>{A^{A~{A�
=A�
=A�=pA�
=A�
=A��
A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bq�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC��C�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM��DNq�DN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDh~�Dh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDy��D��\D�B�D�\D�D��D�5�D���D���D���D�5�D�\Dǿ\D���D�<)D�r�D���D�\D�8�D�e�D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��/A���A�+AԼjA�M�A�(�A� �A�{A�1A�%A�A�  A���A�  A�
=A�$�A� �A�"�A�&�A�/A�33A�-A��A��mAӡ�A�K�A��A��Aҩ�A��A��A�
=A�;dA�Aɥ�A��A���Aȗ�A�=qA�x�A��Aá�A�ZA�+A���A�~�A�%A�Q�A�A��7A���A�Q�A��jA��A�ZA��HA��A�A�Q�A��9A��hA�(�A�-A�(�A�x�A�l�A�v�A���A�{A��A��/A��wA��uA�bNA��jA��jA�r�A���A���A���A�ȴA�v�A��FA�|�A���A�$�A���A���A��jA�ffA��wA�ƨA�n�A�XA�ĜA�;dA��uA��jA���A��HA���A�7LA���A�-A��
A�
=A�A�A�hsA��;A�=qA��\A�ĜA�7LA�^5A~�HA{�TAy�Av�9Au�At�!As�Ao�wAm33Akx�Ai��Ah�HAh^5Ag��Af��Ad��Ac�^Ab�!AaA`A^1A[�mAYƨAX�uAW��AUAT^5AS��ARZAP��AO�;AO/AL��AI�AIS�AI%AG��AE`BAD  AB�HAA�FA@��A@(�A>�HA<jA9�A7/A5�A4=qA1XA/A-�^A-�A,�!A+��A+�hA*�A)S�A&��A&A�A%�7A$��A$$�A#ƨA#A"Q�A!�A!�A!K�A ��A��A�A��A"�A��A�AbNA"�A��A�TA\)A��A�FA�9A5?A�7A��A��A�A�HAffA�7AI�A��A
�uA�/A33A��A��A`BA�jA �A �@���@��7@�Z@���@�V@�@��
@�S�@@��/@�C�@��@�1'@��H@�!@�ff@�-@��@�\@�ff@�G�@��@���@��#@�z�@�  @��@٩�@�&�@ج@�Q�@��@��H@�bN@��@�=q@�@ѩ�@�hs@�bN@��@Ͳ-@�dZ@��H@�x�@���@���@ǶF@��m@Ǿw@�@��@Ł@�I�@þw@���@�O�@�z�@��@��@�;d@�n�@�G�@�j@�"�@���@��^@��@��@�9X@��@��@��@��y@�=q@�J@�&�@�bN@�t�@�33@���@�5?@���@��T@��7@��@�9X@�;d@��@��\@�^5@�{@��-@�O�@�r�@�1@��w@�dZ@�@���@�n�@�$�@���@�G�@�%@���@���@��@�Z@�\)@�
=@��@�v�@��@���@���@��h@�p�@�X@��@���@�z�@�Z@�I�@�1'@��;@���@��!@�V@��@��@���@��-@���@�x�@�/@���@���@��u@�j@�b@��;@��
@�ƨ@���@���@��!@��+@�^5@��@��T@��@��@��^@��h@��@��`@��j@��@�Q�@� �@�1@���@���@�\)@�S�@�S�@�S�@�+@�~�@�V@��@�@�7L@��@�%@��j@�A�@���@���@��F@�"�@��R@��\@�E�@�-@�$�@�{@��T@���@��h@�`B@��`@���@� �@�ƨ@��@���@��@�dZ@�|�@�+@��H@��R@�v�@��!@�n�@�5?@�{@��#@��@�?}@�&�@���@��@��
@�C�@��@���@��\@�v�@�V@�{@��@��^@��7@��@�`B@�G�@�7L@�&�@�%@���@��@���@�33@��@��\@�M�@��#@�G�@�/@�V@�Ĝ@�z�@�Z@�I�@�A�@�ƨ@���@�ƨ@��
@���@�|�@�l�@�\)@�K�@�C�@�33@�"�@��@�@���@�@�x�@�?}@�&�@�V@�V@�%@���@��`@��@�  @w;d@m�@d�@]`B@T�j@L1@D�@=`B@8bN@3�
@-�h@)hs@#dZ@��@�@V@Ĝ@p�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��/A���A�+AԼjA�M�A�(�A� �A�{A�1A�%A�A�  A���A�  A�
=A�$�A� �A�"�A�&�A�/A�33A�-A��A��mAӡ�A�K�A��A��Aҩ�A��A��A�
=A�;dA�Aɥ�A��A���Aȗ�A�=qA�x�A��Aá�A�ZA�+A���A�~�A�%A�Q�A�A��7A���A�Q�A��jA��A�ZA��HA��A�A�Q�A��9A��hA�(�A�-A�(�A�x�A�l�A�v�A���A�{A��A��/A��wA��uA�bNA��jA��jA�r�A���A���A���A�ȴA�v�A��FA�|�A���A�$�A���A���A��jA�ffA��wA�ƨA�n�A�XA�ĜA�;dA��uA��jA���A��HA���A�7LA���A�-A��
A�
=A�A�A�hsA��;A�=qA��\A�ĜA�7LA�^5A~�HA{�TAy�Av�9Au�At�!As�Ao�wAm33Akx�Ai��Ah�HAh^5Ag��Af��Ad��Ac�^Ab�!AaA`A^1A[�mAYƨAX�uAW��AUAT^5AS��ARZAP��AO�;AO/AL��AI�AIS�AI%AG��AE`BAD  AB�HAA�FA@��A@(�A>�HA<jA9�A7/A5�A4=qA1XA/A-�^A-�A,�!A+��A+�hA*�A)S�A&��A&A�A%�7A$��A$$�A#ƨA#A"Q�A!�A!�A!K�A ��A��A�A��A"�A��A�AbNA"�A��A�TA\)A��A�FA�9A5?A�7A��A��A�A�HAffA�7AI�A��A
�uA�/A33A��A��A`BA�jA �A �@���@��7@�Z@���@�V@�@��
@�S�@@��/@�C�@��@�1'@��H@�!@�ff@�-@��@�\@�ff@�G�@��@���@��#@�z�@�  @��@٩�@�&�@ج@�Q�@��@��H@�bN@��@�=q@�@ѩ�@�hs@�bN@��@Ͳ-@�dZ@��H@�x�@���@���@ǶF@��m@Ǿw@�@��@Ł@�I�@þw@���@�O�@�z�@��@��@�;d@�n�@�G�@�j@�"�@���@��^@��@��@�9X@��@��@��@��y@�=q@�J@�&�@�bN@�t�@�33@���@�5?@���@��T@��7@��@�9X@�;d@��@��\@�^5@�{@��-@�O�@�r�@�1@��w@�dZ@�@���@�n�@�$�@���@�G�@�%@���@���@��@�Z@�\)@�
=@��@�v�@��@���@���@��h@�p�@�X@��@���@�z�@�Z@�I�@�1'@��;@���@��!@�V@��@��@���@��-@���@�x�@�/@���@���@��u@�j@�b@��;@��
@�ƨ@���@���@��!@��+@�^5@��@��T@��@��@��^@��h@��@��`@��j@��@�Q�@� �@�1@���@���@�\)@�S�@�S�@�S�@�+@�~�@�V@��@�@�7L@��@�%@��j@�A�@���@���@��F@�"�@��R@��\@�E�@�-@�$�@�{@��T@���@��h@�`B@��`@���@� �@�ƨ@��@���@��@�dZ@�|�@�+@��H@��R@�v�@��!@�n�@�5?@�{@��#@��@�?}@�&�@���@��@��
@�C�@��@���@��\@�v�@�V@�{@��@��^@��7@��@�`B@�G�@�7L@�&�@�%@���@��@���@�33@��@��\@�M�@��#@�G�@�/@�V@�Ĝ@�z�@�Z@�I�@�A�@�ƨ@���@�ƨ@��
@���@�|�@�l�@�\)@�K�@�C�@�33@�"�@��@�@���@�@�x�@�?}@�&�@�V@�V@�%@���@��`@��@�  @w;d@m�@d�@]`B@T�j@L1@D�@=`B@8bN@3�
@-�h@)hs@#dZ@��@�@V@Ĝ@p�@	�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�TB�TB�NB�TB�`B�mB�sB�sB�sB�yB�B�B�B�B��B(�BH�BjBt�By�B~�B�%B�JB��B��B��B��B�B�!BÖB�sB7LBC�BL�B\)BgmBn�Bm�Bm�Bk�Bk�BhsBgmBe`BcTB_;B]/B\)BZBYBT�BL�B33BhB+B%B��B�B�5B��BƨB�RB�B��B��B��B��B��B��B�dB�^B�!B�B��B��B�hB{�Bo�Be`B^5BO�B<jB1'B/B%�B�BPB��B�BB�}B��B��B�Bt�Bk�BS�B>wB49B0!B&�B�B
��B
�ZB
�B
��B
ŢB
�XB
�B
��B
��B
�VB
�B
y�B
l�B
]/B
E�B
0!B
�B
�B
hB
+B	��B	�`B	�B	��B	ƨB	B	�jB	�?B	��B	��B	��B	�VB	�B	t�B	iyB	^5B	W
B	P�B	H�B	B�B	>wB	9XB	33B	.B	)�B	 �B	�B	uB	hB	DB	B��B��B��B�B�B�yB�BB�
B��B��BĜB�qB�RB�3B�-B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�\B�PB�JB�DB�=B�7B�1B�+B�%B�B�B�B}�B|�By�Bu�Bn�BiyBe`BaHB_;B]/B]/B]/B\)BZBZBZBXBW
BVBT�BR�BP�BM�BI�BH�BG�BE�BA�B>wBB�BE�BH�BI�BK�BL�BW
Bl�Bm�Bn�Bq�Br�Bt�B{�B�B�B�B�%B�+B�+B�%B�%B�+B�JB�VB�\B�VB�VB�bB�oB�JB�Bw�Bs�Bq�By�B��B��B��B��B��B��B�B�B�!B�9B�LB�XB�wB�}B�}B�}BÖBɺB��B��B��B��B��B��B��B�B�B�;B�HB�ZB�sB�B�B�B��B��B��B��B	  B	+B	PB	\B	oB	{B	�B	�B	�B	#�B	&�B	(�B	,B	/B	2-B	49B	6FB	9XB	=qB	>wB	@�B	B�B	B�B	C�B	J�B	L�B	L�B	Q�B	VB	W
B	YB	ZB	[#B	[#B	\)B	_;B	`BB	bNB	dZB	ffB	iyB	jB	o�B	q�B	r�B	s�B	t�B	u�B	u�B	v�B	x�B	z�B	{�B	}�B	~�B	�B	�B	�B	�B	�B	�7B	�DB	�PB	�VB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�9B	�LB	�RB	�RB	�^B	�qB	�wB	�}B	��B	ÖB	ƨB	ƨB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�#B	�/B	�TB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
1B
1B
1B
1B

=B
DB
DB
JB
PB
PB
PB
PB
VB
oB
�B
�B
%�B
-B
49B
:^B
A�B
G�B
L�B
O�B
T�B
ZB
^5B
cTB
gmB
k�B
p�B
t�B
w�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�^B�\B�WB�_B�iB�uB�}B�}B�|B�B�B�B�B�B��B(�BH�Bj�Bt�By�BB�1B�SB��B��B��B��B�B�+BâB�B7XBC�BL�B\8Bg}Bn�Bm�Bm�Bk�Bk�Bh�Bg|BelBc`B_JB]<B\7BZ+BY'BU
BL�B3ABvB7B1B��B�B�BB�BƳB�_B�(B��B��B��B��B��B��B�oB�kB�-B�B�B��B�rB{�Bo�BeiB^?BO�B<vB10B/$B%�B�B[B��B�PB��B��B��B�#Bt�Bk�BTB>�B4HB00B&�B�B
��B
�jB
�,B
��B
ųB
�fB
�B
��B
��B
�gB
�"B
y�B
l�B
]?B
E�B
01B
�B
�B
zB
@B	��B	�vB	�$B	��B	ƿB	¥B	�B	�UB	�B	��B	��B	�nB	�B	t�B	i�B	^NB	W%B	P�B	H�B	B�B	>�B	9qB	3OB	.0B	*B	 �B	�B	�B	�B	aB	/B�B��B��B��B�B�B�cB�)B�B��BĿB��B�qB�TB�PB�IB�@B�.B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�qB�lB�iB�_B�ZB�UB�QB�HB�?B�4B�,B~B}Bz Bu�Bn�Bi�Be�BaoB_aB]SB]VB]SB\NBZCBZDBZCBX6BW/BV+BU"BSBQBM�BI�BH�BG�BE�BA�B>�BB�BE�BH�BI�BK�BL�BW/Bl�Bm�Bn�Bq�Br�Bt�B|B�,B�)B�7B�HB�LB�QB�HB�HB�PB�mB�xB��B�yB�yB��B��B�nB�=Bw�Bs�Bq�Bz B��B��B��B��B�B�B�0B�/B�DB�ZB�nB�xB��B��B��B��BõB��B��B��B��B�	B�B�B�B�%B�6B�YB�hB�yB�B�B��B��B��B��B��B�B	  B	JB	nB	yB	�B	�B	�B	�B	�B	#�B	'B	)B	,$B	/6B	2JB	4YB	6`B	9sB	=�B	>�B	@�B	B�B	B�B	C�B	J�B	L�B	L�B	RB	V!B	W&B	Y4B	Z:B	[AB	[=B	\EB	_VB	`^B	bjB	dtB	fB	i�B	j�B	o�B	q�B	r�B	s�B	t�B	u�B	u�B	v�B	x�B	z�B	|B	~B	B	�&B	�.B	�3B	�3B	�8B	�OB	�\B	�iB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�"B	�%B	�-B	�:B	�@B	�EB	�PB	�fB	�lB	�kB	�vB	��B	��B	��B	��B	îB	ƾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�"B	�(B	�/B	�-B	�9B	�FB	�kB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�B	�B	�B
#B
(B
5B
IB
HB
	LB
	MB
	LB
	LB
	MB
	LB
IB
FB
FB
HB

SB
YB
\B
_B
fB
fB
fB
dB
lB
�B
�B
�B
%�B
-%B
4NB
:sB
A�B
G�B
L�B
O�B
UB
Z/B
^IB
cgB
g�B
k�B
p�B
t�B
w�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214262016053112142620160531121426  AO  ARCAADJP                                                                    20140721230513    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230513  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230513  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121426  IP                  G�O�G�O�G�O�                