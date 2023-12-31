CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-08-20T17:02:03Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20170820170203  20190604094029  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @��(d1d1   @��� N|@5��-V�d¸Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0ffB8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�qD��D�7�D�xRD�g
D�D�EqD���D��HD���D�9�D���D�׮D��D�EqDڑ�DྸD��D�6�D�D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@xQ�@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A��
A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�Dy��D��D�3�D�t{D�c3D�=D�A�D�}D��qD��D�5�D���D���D�D�A�DڎD��D��=D�2�D��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��\A��PA��DA��\A���A���A���A���A���A���A��A��A��A��A��A��A��uA�`BA�  A�XA��A���A�$�A֮AՓuA�dZA��/Aҙ�A�
=A�Q�A̡�A��HA�x�A��
A�9XA�p�A���A£�A���A�z�A���A��A���A�ffA��!A�7LA��A�  A��A�hsA�G�A�+A���A�%A�E�A�$�A���A�x�A��HA�^5A���A�bA�l�A�?}A��+A���A�oA��#A�+A�VA�^5A��!A���A�v�A�K�A���A���A�ƨA�dZA�$�A��uA��A�;dA�(�A���A�ZA�ȴA��yA�ȴA�dZA��yA��^A��A�ZA��`A��jA��uA�`BA��A�^5A� �A�hsA���A�C�A���A�-A��A~�jA}XAy�-Av��At��Aq��Am�#Al9XAjE�Ag33Ad�RAcƨAcVAb�A`�\A`�A_��A^Q�A]dZA\��A[`BAZZAX�AUC�AQ�AO��AM;dAM/AMoAL�AK��AJ��AH��AHI�AG��AG��AG\)AGC�AFv�AE�;AE?}AD �ABbNA@^5A=�FA< �A<1A;K�A8�A6JA3�A2�DA1C�A0bA.�\A-7LA,��A+�mA+C�A)�A'�A%|�A#��A!�A�A{A��A�A��A  AhsAO�A?}A^5AdZAx�A�7A�HA1'A�7A��AƨA�A~�AjA��A"�A�yA��A�AA
��A
bA	XA	C�A	An�Ax�A��A�A��A
=A��A��A �yA bN@�\)@�~�@�?}@�Z@���@�K�@��@���@�r�@��y@�h@�&�@��@�ff@��y@���@�z�@�S�@�-@�@噚@�I�@㝲@�t�@��`@ާ�@ݡ�@�I�@���@��@�~�@�ff@֏\@��H@�-@ԛ�@ӕ�@���@�O�@υ@�Ĝ@ʰ!@���@�o@���@�Ĝ@�ƨ@�S�@§�@�=q@�@���@��@�@�O�@�j@�"�@�J@��@�j@��w@�;d@���@��@��@�1@��w@���@�;d@���@�~�@�~�@�v�@�ff@�$�@��@���@��h@�`B@��`@�b@��
@�ƨ@��P@�\)@�"�@�@���@���@�x�@�G�@��`@���@�9X@��;@�t�@�K�@�K�@��@���@�-@��^@�`B@�%@�j@��@�;d@���@���@�`B@���@��@��u@�r�@��m@�|�@�+@��@��@��@�&�@��`@�Z@���@��@�S�@�o@��y@��R@�M�@��#@��@�bN@���@���@��@�l�@�\)@�;d@�o@��R@�ff@�J@��@���@�hs@�&�@��@�I�@�1@���@��m@���@���@�\)@�C�@�+@�+@�@��y@�ȴ@�ȴ@���@�n�@�
=@��!@�=q@�{@�J@�-@�J@��#@�@���@�X@���@��@���@��u@��u@��D@��@�j@�I�@�\)@���@�ȴ@�n�@��T@��7@�x�@�x�@��@���@��-@�p�@��@���@��D@�z�@��D@��u@���@���@��9@�Z@�9X@�1'@�b@�  @��m@���@��P@�t�@�dZ@�\)@�S�@�S�@�S�@�S�@�33@��y@��\@�n�@��@���@���@�O�@�7L@�/@��@�%@���@��@���@��@���@��D@��D@�Z@�Z@�(�@��@�K�@�o@��@���@��@��@���@��@�p�@�G�@�/@�/@�%@��`@�Ĝ@��u@�1'@�Z@�r�@�1'@���@��;@�l�@�+@�
=@���@�ff@�$�@���@��T@���@�@�@���@�8@��A@w�}@nQ@dh�@]%F@U�S@MDg@F�H@>�A@6ߤ@0�@(֡@$	�@�#@Q�@u�@[�@O�@�p1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 A��A��\A��PA��DA��\A���A���A���A���A���A���A��A��A��A��A��A��A��uA�`BA�  A�XA��A���A�$�A֮AՓuA�dZA��/Aҙ�A�
=A�Q�A̡�A��HA�x�A��
A�9XA�p�A���A£�A���A�z�A���A��A���A�ffA��!A�7LA��A�  A��A�hsA�G�A�+A���A�%A�E�A�$�A���A�x�A��HA�^5A���A�bA�l�A�?}A��+A���A�oA��#A�+A�VA�^5A��!A���A�v�A�K�A���A���A�ƨA�dZA�$�A��uA��A�;dA�(�A���A�ZA�ȴA��yA�ȴA�dZA��yA��^A��A�ZA��`A��jA��uA�`BA��A�^5A� �A�hsA���A�C�A���A�-A��A~�jA}XAy�-Av��At��Aq��Am�#Al9XAjE�Ag33Ad�RAcƨAcVAb�A`�\A`�A_��A^Q�A]dZA\��A[`BAZZAX�AUC�AQ�AO��AM;dAM/AMoAL�AK��AJ��AH��AHI�AG��AG��AG\)AGC�AFv�AE�;AE?}AD �ABbNA@^5A=�FA< �A<1A;K�A8�A6JA3�A2�DA1C�A0bA.�\A-7LA,��A+�mA+C�A)�A'�A%|�A#��A!�A�A{A��A�A��A  AhsAO�A?}A^5AdZAx�A�7A�HA1'A�7A��AƨA�A~�AjA��A"�A�yA��A�AA
��A
bA	XA	C�A	An�Ax�A��A�A��A
=A��A��A �yA bN@�\)@�~�@�?}@�Z@���@�K�@��@���@�r�@��y@�h@�&�@��@�ff@��y@���@�z�@�S�@�-@�@噚@�I�@㝲@�t�@��`@ާ�@ݡ�@�I�@���@��@�~�@�ff@֏\@��H@�-@ԛ�@ӕ�@���@�O�@υ@�Ĝ@ʰ!@���@�o@���@�Ĝ@�ƨ@�S�@§�@�=q@�@���@��@�@�O�@�j@�"�@�J@��@�j@��w@�;d@���@��@��@�1@��w@���@�;d@���@�~�@�~�@�v�@�ff@�$�@��@���@��h@�`B@��`@�b@��
@�ƨ@��P@�\)@�"�@�@���@���@�x�@�G�@��`@���@�9X@��;@�t�@�K�@�K�@��@���@�-@��^@�`B@�%@�j@��@�;d@���@���@�`B@���@��@��u@�r�@��m@�|�@�+@��@��@��@�&�@��`@�Z@���@��@�S�@�o@��y@��R@�M�@��#@��@�bN@���@���@��@�l�@�\)@�;d@�o@��R@�ff@�J@��@���@�hs@�&�@��@�I�@�1@���@��m@���@���@�\)@�C�@�+@�+@�@��y@�ȴ@�ȴ@���@�n�@�
=@��!@�=q@�{@�J@�-@�J@��#@�@���@�X@���@��@���@��u@��u@��D@��@�j@�I�@�\)@���@�ȴ@�n�@��T@��7@�x�@�x�@��@���@��-@�p�@��@���@��D@�z�@��D@��u@���@���@��9@�Z@�9X@�1'@�b@�  @��m@���@��P@�t�@�dZ@�\)@�S�@�S�@�S�@�S�@�33@��y@��\@�n�@��@���@���@�O�@�7L@�/@��@�%@���@��@���@��@���@��D@��D@�Z@�Z@�(�@��@�K�@�o@��@���@��@��@���@��@�p�@�G�@�/@�/@�%@��`@�Ĝ@��u@�1'@�Z@�r�@�1'@���@��;@�l�@�+@�
=@���@�ff@�$�@���@��T@���@�G�O�@���@�8@��A@w�}@nQ@dh�@]%F@U�S@MDg@F�H@>�A@6ߤ@0�@(֡@$	�@�#@Q�@u�@[�@O�@�p1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�-B�-B�-B�-B�-B�-B�-B�-B�-B�-B�3B�3B�3B�3B�3B�-B�RB�}B��B��B�B+BP�BhsB{�B�B�uB��B��B��B��B�9B�dB�}B�}B�}B�}B�}B�jB�wB��B�}B��BÖB��BÖB�LBB��B��BBBB��B��B�jB�-B��B��B��B�hB�DB�%B{�Br�Bm�BffBbNB_;BXBN�BD�B6FB �B{B1B  B�B�yB�mB�#B��B��B�B��B��B�DB�Bw�Bt�Bn�B\)BK�B<jB/B(�B%�B"�B�BPB
��B
�B
�HB
�
B
��B
�?B
��B
�JB
�B
t�B
[#B
E�B
49B
�B
B	��B	�mB	��B	B	�dB	�LB	�!B	��B	��B	��B	��B	�oB	�VB	�B	{�B	n�B	]/B	J�B	E�B	:^B	9XB	7LB	49B	-B	%�B	�B	�B	�B	�B	�B	�B	uB	hB	bB	JB	%B��B�B�yB�mB�ZB�/B��B��B��B��BɺBŢBÖB��B�}B�jB�LB�'B�B��B��B��B��B��B�uB�PB�1B�+B�%B�B�B�B~�B}�B�B� B}�B{�Bw�Bs�Bq�Bt�Bs�Bq�Bp�Bn�Bp�Bo�Bl�BjBhsBgmBdZBcTBaHB`BB^5B^5B]/B^5B^5B_;B]/B\)BZBZBYBXBXBYBYBXBZB[#B[#B\)B]/BaHBbNBdZBe`BffBffBffBhsBhsBffBdZBcTBaHB^5BaHBdZBffBhsBo�By�B|�B�B�B�B�B�+B�7B�PB�hB��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�9B�LB�^B�jB�qBBĜB��B��B��B��B�
B�#B�)B�/B�5B�5B�BB�HB�NB�TB�ZB�`B�sB�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	B	B	B	B	+B		7B	
=B	JB	VB	hB	uB	{B	�B	�B	�B	�B	�B	�B	�B	"�B	&�B	'�B	(�B	.B	0!B	1'B	7LB	9XB	:^B	;dB	<jB	;dB	;dB	<jB	?}B	A�B	C�B	F�B	G�B	H�B	I�B	J�B	K�B	M�B	P�B	S�B	XB	ZB	[#B	aHB	aHB	bNB	cTB	K�B	ffB	m�B	t�B	x�B	|�B	}�B	}�B	}�B	~�B	�B	�B	�DB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�-B	�-B	�-B	�3B	�?B	�FB	�FB	�?B	�9B	�9B	�9B	�9B	�?B	�FB	�RB	�XB	�dB	�jB	�wB	�wB	��B	ÖB	ŢB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�B	�/B	�5B	�5B	�;B	�BB	�BB	�NB	�NB	�ZB	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
zB
�B
kB
 BB
%,B
4�B
;dB
?cB
EmB
J�B
Q�B
R�B
W
B
]IB
b4B
eFB
n}B
rB
w2B
{B
~]1111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�aB�_B�B jBFKB]�BqIBwrB��B��B��B��B�JB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��G�O�B��B��B��B��B��B��B��B��B��B��B�MB�B��B��B��B{�BqUBhBcB[�BW�BT�BM~BDGB:B+�B9B	�B��B�uB�/B��B��BСB�fB�B��B�JB�B��Bx�BmWBjDBdBQ�BASB1�B$�B�BrBdB>B�B
�vB
�B
��B
̠B
�B
��B
�ZB
��B
v�B
j[B
P�B
;FB
)�B
XB	��B	�}B	�B	ȠB	�@B	�B	��B	��B	��B	��B	�yB	�IB	�#B	�B	y�B	q�B	dRB	R�B	@~B	;\B	0B	/B	-B	)�B	"�B	�B	lB	aB	^B	VB	NB	BB		9B	'B	$B	B��B�B�vB�?B�5B�B��B��BƲBģBB��B�mB�cB�TB�HB�8B�B��B��B��B��B�iB�`B�RB�IB�%B~B|�B{�By�Bw�By�Bt�Bs�Bv�Bu�Bs�Bq�Bm�Bi�Bg�Bj�Bi�Bg�Bf{BdmBfxBesBbeB`VB^PB]HBZ5BY.BW#BVBTBTBSBTBTBUBSBRBO�BO�BN�BM�BM�BN�BN�BM�BO�BP�BQ BRBS	BW%BX(BZ5B[<B\?B\BB\CB^PB^NB\FBZ6BY-BW&BTBW$BZ7B\CB^PBexBo�Br�Bv�Bv�Bv�By�B}BB�,B�CB�cB�pB��B��B��B��B��B��B��B��B��B��B��B��B�B�%B�6B�@B�HB�fB�xB��B��B��B��B��B��B� B�B�B�B�B� B�%B�'B�2B�3B�IB�TB�UB�_B�`B�tB�B�B�B��B��B�B�B��B��B��B��B��B��B��B� B�B	 B	B	)B	<B		KB	
OB	aB	nB	zB	zB	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	&�B	-B	/'B	01B	15B	2;B	13B	14B	2>B	5LB	7]B	9jB	<zB	=�B	>�B	?�B	@�B	A�B	C�B	F�B	I�B	M�B	O�B	P�B	WB	WB	XB	Y"G�O�B	\4B	c_B	j�B	n�B	r�B	s�B	s�B	s�B	t�B	v�B	z�B	�B	�%B	�;B	�_B	�rB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�0B	�3B	�>B	�AB	�LB	�`B	�nB	�qB	�lB	��B	ÝB	ŦB	ǳB	ȷB	ɿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�(B	�&B	�)B	�0B	�1B	�1B	�6B	�?B	�EB	�LB	�KB	�GB	�FB	�QB	�_B	�ZB	�\B	�]B	�gB	�mB	�}B	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�>B
�B
0B
B
�B
*gB
1&B
5#B
;2B
@MB
GDB
H}B
L�B
SB
W�B
[B
d=B
g�B
l�B
p�B
t 1111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.12 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0), vertically averaged dS =-0.01(+/-0.001) in PSS-78.                                                                                                                                                                                             Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040940292019060409402920190604094029  AO  ARCAADJP                                                                    20170820170203    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170820170203  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170820170203  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094029  IP                  G�O�G�O�G�O�                