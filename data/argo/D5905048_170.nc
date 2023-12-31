CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-10-16T00:35:30Z creation;2017-10-16T00:35:33Z conversion to V3.1;2019-12-19T07:54:58Z update;     
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
resolution        =���   axis      Z        h  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  `$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  sh   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20171016003530  20200116221515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_170                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�-����1   @�-�β@�@4�C��%�dĵ'�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D�� D�� D�  D�@ D�� D�ɚ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @xQ�@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCIǮCKǮCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�?\D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��\D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D���D�<)D�|)D��)D��)D�<)D�|)D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�5?A�+A�(�A��A��A�VA�
=A�1A�A�A���A��yA��A��A��HA��#A��
A���A�ƨAܸRAܟ�A�/A�ffA�ZA��TA�oA���A˓uA�n�AȁA�VA�=qA�1'A�+A���AƟ�A�+A�\)A���A�hsA�VA��A�bNA�p�A���A� �A�bA�oA��7A�{A��+A���A��TA�ĜA�M�A��A���A�`BA���A�hsA�+A���A��TA�oA���A��A�^5A��A�
=A�O�A���A��uA�$�A��^A�VA� �A��PA�S�A��A��`A�ffA�{A�ĜA�M�A���A��A��yA��+A��PA�ffA��A��mA��A�$�A��yA���A�dZA��A��7A��#A�jA��PA�K�A�`BA��^A���A��A�p�A�9XA�1A�ȴA��A�JA�9XA�  A�dZA��mA�ZA���A~Az�RAx5?AuS�At5?AoO�Ak��Ai`BAg;dAf~�Ae�TAb�HAaXA]|�A\E�AZ=qAYl�AY;dAYoAWK�AU��AT �ARjAQdZAO��AN��AM��AL�+AK�AI|�AGhsAE�-ADȴADr�AC�^AB(�A@��A?hsA>~�A=dZA<�9A<{A;+A:ZA9�A7��A6��A6z�A5�-A5+A4�!A3ƨA2^5A0�A/��A.�A,v�A)�A'��A'XA&9XA%�A#�A!O�A v�AoA?}A�uA�A�wA��AJA�HAS�A�wAE�Ap�A�AVA�A1'A
��A
n�A	�#A��A��A��Az�AZAbA�AE�A��AC�A�FA ��A M�@�n�@��P@���@�"�@�{@�`B@�K�@���@�\)@�p�@�;d@�-@�Z@���@��@�X@��@�j@��m@�S�@���@��y@⟾@�n�@�Ĝ@���@ޗ�@ޟ�@�n�@�J@ݲ-@ݑh@�`B@�Q�@ۥ�@�
=@��T@ف@��@؛�@�l�@�`B@�(�@ӶF@�C�@ҧ�@с@�|�@Η�@�/@��@���@�ff@��@��@�ƨ@���@���@���@�\)@���@�ȴ@�~�@��h@�r�@��
@�t�@��!@�V@��h@��@�A�@�C�@��@�7L@�%@���@���@���@���@���@�r�@�A�@���@�C�@�o@��y@�E�@���@��h@�/@���@���@��@�I�@�(�@��
@���@�\)@�|�@�t�@�
=@�^5@��@�o@��!@���@�C�@�9X@��@��@���@��!@�X@���@���@�dZ@��H@���@��!@��@�ȴ@�V@�"�@���@�$�@�M�@���@���@��7@�hs@�X@�G�@��@���@��j@�9X@�|�@�"�@�
=@���@��@���@��@��@��@�n�@��@�@���@��-@�G�@��@���@���@�1@��F@��@�"�@��R@���@�ff@�E�@�M�@��@���@��@�X@���@���@��u@�z�@�j@�Z@�1'@��@�1@�  @�  @�  @��@�ƨ@���@�t�@�;d@�
=@��H@���@���@�~�@�^5@�5?@���@�@��7@���@��/@���@��D@��D@��D@��u@��D@�r�@�Q�@��@���@���@���@�t�@�\)@�K�@�C�@�"�@�@��y@�ȴ@�-@��@���@�`B@�/@���@��j@��D@�(�@�b@��@���@�|�@�"�@���@���@�v�@�{@��@���@�7L@��@��/@��@�Q�@�b@���@���@�|�@�K�@�33@�"�@��@�ff@�-@���@�x�@�7L@�&�@��@�V@��/@��@�1@�33@�-@��@���@�J@�J@���@��#@���@��@�X@�X@�/@��@��/@���@�(�@���@��F@��@�C�@��@���@�M�@�5?@�J@��#@��^@��7@�`B@��j@���@�33@��H@��+@�{@��@��@���@��7@�`B@�X@�Ĝ@�Q�@�  @���@���@�\)@�;d@���@�n�@�V@�-@��T@��7@��@��j@���@��@���@�r�@�(�@�  @��@\)@;d@+@
=@~��@~�+@~5?@}�T@}�@}V@|Z@{��@{�
@{"�@z��@yG�@xQ�@x  @w+@v�R@v5?@u�@u@u�@t�j@tZ@s��@sƨ@t9X@s�@q��@o�;@o��@o�P@o\)@o;d@o�@o
=@n�y@n�R@nȴ@n�@n��@n�+@n5?@m�@l�D@l�@kƨ@k�@k33@k"�@j�H@j�!@jM�@jJ@i��@ihs@i7L@i%@h�`@hĜ@h�u@hbN@h �@g�w@g;d@g
=@f�y@fȴ@fE�@e@e�@eO�@eO�@e?}@d�j@dI�@co@b��@b=q@a�@a�7@`��@`bN@`Q�@`1'@`b@_�w@_K�@_
=@^ȴ@^�+@^$�@]O�@]V@\�/@\j@[�
@[C�@Z�!@Z�\@Z�!@Z~�@Y�@Y�^@Y%@W�w@V�@V�R@VE�@U@U�@UO�@T�@T�@TZ@T(�@S��@SS�@R�\@Q�@Q��@Q��@Q��@Q�7@Qhs@P�`@PA�@O�;@Ol�@Nȴ@Nv�@N5?@M��@Mp�@M/@L��@L��@L�D@L�D@L�D@Lz�@L9X@Kƨ@K�F@K��@KdZ@K33@J~�@I��@I�#@I��@Ix�@I7L@I�@H��@H�9@HQ�@G�;@G|�@G+@G�@F��@F�@F��@F��@Fff@E�@E@E�-@E�@E`B@EO�@E?}@E�@D�@D�@D(�@C�m@C�m@C�
@C�F@CC�@B^5@A�#@Ahs@AX@AX@AG�@A7L@A�@@��@@�`@@��@@�@@r�@@r�@@r�@@r�@@Q�@@ �@?�w@?;d@?
=@>�@>ȴ@>�R@>�+@>V@>$�@>@=�@=@=/@<�j@<I�@<�@;�m@;��@;�@;33@:�\@:^5@:J@9��@9�7@9x�@9x�@9hs@9X@9X@9G�@97L@9&�@9&�@9�@8 �@7l�@7
=@6v�@6$�@6@5�T@5��@5O�@5/@4�@4�j@49X@3ƨ@3��@3C�@2�H@2�\@1��@1�7@1&�@0��@0Ĝ@0�9@0�9@0�u@0bN@/�@/��@/�P@/l�@/;d@.�y@.ȴ@.��@.��@.ff@-�T@-`B@,��@,�@,��@,�@,�@,��@,�D@,z�@,Z@+�
@+��@+�@+S�@+"�@+o@*��@*�@)�7@)hs@)G�@)7L@)%@(�`@(��@(Q�@'�;@'�P@&�R@&{@%��@%��@%�@%?}@$��@$�D@#�
@#�F@#t�@#o@"�@"�!@!��@!�^@!�^@!��@!��@!x�@!x�@!hs@!X@!G�@!�@ ��@ Ĝ@ b@�@�;@l�@;d@�@
=@
=@
=@��@�@�R@�+@�+@��@��@��@�+@V@$�@��@�h@O�@?}@?}@/@�@��@��@��@�D@z�@Z@�F@t�@S�@C�@C�@33@"�@@n�@J@��@��@X@%@�`@��@r�@ �@  @�@��@�@��@��@��@��@|�@l�@+@��@�+@v�@ff@E�@{@�@��@��@`B@�@�@�D@j@j@I�@�m@��@dZ@"�@o@@�@�H@��@��@�!@��@M�@J@��@�@G�@�`@Ĝ@�9@��@�u@�@r�@Q�@A�@ �@b@  @�;@�@|�@;d@
=@��@��@
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�5?A�+A�(�A��A��A�VA�
=A�1A�A�A���A��yA��A��A��HA��#A��
A���A�ƨAܸRAܟ�A�/A�ffA�ZA��TA�oA���A˓uA�n�AȁA�VA�=qA�1'A�+A���AƟ�A�+A�\)A���A�hsA�VA��A�bNA�p�A���A� �A�bA�oA��7A�{A��+A���A��TA�ĜA�M�A��A���A�`BA���A�hsA�+A���A��TA�oA���A��A�^5A��A�
=A�O�A���A��uA�$�A��^A�VA� �A��PA�S�A��A��`A�ffA�{A�ĜA�M�A���A��A��yA��+A��PA�ffA��A��mA��A�$�A��yA���A�dZA��A��7A��#A�jA��PA�K�A�`BA��^A���A��A�p�A�9XA�1A�ȴA��A�JA�9XA�  A�dZA��mA�ZA���A~Az�RAx5?AuS�At5?AoO�Ak��Ai`BAg;dAf~�Ae�TAb�HAaXA]|�A\E�AZ=qAYl�AY;dAYoAWK�AU��AT �ARjAQdZAO��AN��AM��AL�+AK�AI|�AGhsAE�-ADȴADr�AC�^AB(�A@��A?hsA>~�A=dZA<�9A<{A;+A:ZA9�A7��A6��A6z�A5�-A5+A4�!A3ƨA2^5A0�A/��A.�A,v�A)�A'��A'XA&9XA%�A#�A!O�A v�AoA?}A�uA�A�wA��AJA�HAS�A�wAE�Ap�A�AVA�A1'A
��A
n�A	�#A��A��A��Az�AZAbA�AE�A��AC�A�FA ��A M�@�n�@��P@���@�"�@�{@�`B@�K�@���@�\)@�p�@�;d@�-@�Z@���@��@�X@��@�j@��m@�S�@���@��y@⟾@�n�@�Ĝ@���@ޗ�@ޟ�@�n�@�J@ݲ-@ݑh@�`B@�Q�@ۥ�@�
=@��T@ف@��@؛�@�l�@�`B@�(�@ӶF@�C�@ҧ�@с@�|�@Η�@�/@��@���@�ff@��@��@�ƨ@���@���@���@�\)@���@�ȴ@�~�@��h@�r�@��
@�t�@��!@�V@��h@��@�A�@�C�@��@�7L@�%@���@���@���@���@���@�r�@�A�@���@�C�@�o@��y@�E�@���@��h@�/@���@���@��@�I�@�(�@��
@���@�\)@�|�@�t�@�
=@�^5@��@�o@��!@���@�C�@�9X@��@��@���@��!@�X@���@���@�dZ@��H@���@��!@��@�ȴ@�V@�"�@���@�$�@�M�@���@���@��7@�hs@�X@�G�@��@���@��j@�9X@�|�@�"�@�
=@���@��@���@��@��@��@�n�@��@�@���@��-@�G�@��@���@���@�1@��F@��@�"�@��R@���@�ff@�E�@�M�@��@���@��@�X@���@���@��u@�z�@�j@�Z@�1'@��@�1@�  @�  @�  @��@�ƨ@���@�t�@�;d@�
=@��H@���@���@�~�@�^5@�5?@���@�@��7@���@��/@���@��D@��D@��D@��u@��D@�r�@�Q�@��@���@���@���@�t�@�\)@�K�@�C�@�"�@�@��y@�ȴ@�-@��@���@�`B@�/@���@��j@��D@�(�@�b@��@���@�|�@�"�@���@���@�v�@�{@��@���@�7L@��@��/@��@�Q�@�b@���@���@�|�@�K�@�33@�"�@��@�ff@�-@���@�x�@�7L@�&�@��@�V@��/@��@�1@�33@�-@��@���@�J@�J@���@��#@���@��@�X@�X@�/@��@��/@���@�(�@���@��F@��@�C�@��@���@�M�@�5?@�J@��#@��^@��7@�`B@��j@���@�33@��H@��+@�{@��@��@���@��7@�`B@�X@�Ĝ@�Q�@�  @���@���@�\)@�;d@���@�n�@�V@�-@��T@��7@��@��j@���@��@���@�r�@�(�@�  @��@\)@;d@+@
=@~��@~�+@~5?@}�T@}�@}V@|Z@{��@{�
@{"�@z��@yG�@xQ�@x  @w+@v�R@v5?@u�@u@u�@t�j@tZ@s��@sƨ@t9X@s�@q��@o�;@o��@o�P@o\)@o;d@o�@o
=@n�y@n�R@nȴ@n�@n��@n�+@n5?@m�@l�D@l�@kƨ@k�@k33@k"�@j�H@j�!@jM�@jJ@i��@ihs@i7L@i%@h�`@hĜ@h�u@hbN@h �@g�w@g;d@g
=@f�y@fȴ@fE�@e@e�@eO�@eO�@e?}@d�j@dI�@co@b��@b=q@a�@a�7@`��@`bN@`Q�@`1'@`b@_�w@_K�@_
=@^ȴ@^�+@^$�@]O�@]V@\�/@\j@[�
@[C�@Z�!@Z�\@Z�!@Z~�@Y�@Y�^@Y%@W�w@V�@V�R@VE�@U@U�@UO�@T�@T�@TZ@T(�@S��@SS�@R�\@Q�@Q��@Q��@Q��@Q�7@Qhs@P�`@PA�@O�;@Ol�@Nȴ@Nv�@N5?@M��@Mp�@M/@L��@L��@L�D@L�D@L�D@Lz�@L9X@Kƨ@K�F@K��@KdZ@K33@J~�@I��@I�#@I��@Ix�@I7L@I�@H��@H�9@HQ�@G�;@G|�@G+@G�@F��@F�@F��@F��@Fff@E�@E@E�-@E�@E`B@EO�@E?}@E�@D�@D�@D(�@C�m@C�m@C�
@C�F@CC�@B^5@A�#@Ahs@AX@AX@AG�@A7L@A�@@��@@�`@@��@@�@@r�@@r�@@r�@@r�@@Q�@@ �@?�w@?;d@?
=@>�@>ȴ@>�R@>�+@>V@>$�@>@=�@=@=/@<�j@<I�@<�@;�m@;��@;�@;33@:�\@:^5@:J@9��@9�7@9x�@9x�@9hs@9X@9X@9G�@97L@9&�@9&�@9�@8 �@7l�@7
=@6v�@6$�@6@5�T@5��@5O�@5/@4�@4�j@49X@3ƨ@3��@3C�@2�H@2�\@1��@1�7@1&�@0��@0Ĝ@0�9@0�9@0�u@0bN@/�@/��@/�P@/l�@/;d@.�y@.ȴ@.��@.��@.ff@-�T@-`B@,��@,�@,��@,�@,�@,��@,�D@,z�@,Z@+�
@+��@+�@+S�@+"�@+o@*��@*�@)�7@)hs@)G�@)7L@)%@(�`@(��@(Q�@'�;@'�P@&�R@&{@%��@%��@%�@%?}@$��@$�D@#�
@#�F@#t�@#o@"�@"�!@!��@!�^@!�^@!��@!��@!x�@!x�@!hs@!X@!G�@!�@ ��@ Ĝ@ b@�@�;@l�@;d@�@
=@
=@
=@��@�@�R@�+@�+@��@��@��@�+@V@$�@��@�h@O�@?}@?}@/@�@��@��@��@�D@z�@Z@�F@t�@S�@C�@C�@33@"�@@n�@J@��@��@X@%@�`@��@r�@ �@  @�@��@�@��@��@��@��@|�@l�@+@��@�+@v�@ff@E�@{@�@��@��@`B@�@�@�D@j@j@I�@�m@��@dZ@"�@o@@�@�H@��@��@�!@��@M�@J@��@�@G�@�`@Ĝ@�9@��@�u@�@r�@Q�@A�@ �@b@  @�;@�@|�@;d@
=@��@��@
=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�^B�dB�dB�dB�^B�^B�^B�XB�XB�XB�LB�FB�FB�FB�9B�9B�?B�?B�RB�jB��B�-B��B��B��B��B��B�B�B(�B-B0!B0!B,B+B>wBH�BH�BN�BO�BF�BK�BH�BJ�BQ�BP�BYBe`Bl�BhsBffBjBgmBaHBbNBk�Bw�Bu�B{�Bz�Bu�Bs�Bs�Bw�Bs�Bv�Bt�Bk�Bk�Bl�BiyBgmBdZB]/BS�BL�BK�BB�B)�B�BuBB�B�BB�5B�B��BĜB�B��B��B�bB�JB�=B�+B�Bz�Br�BgmB[#BF�B.B(�BhB	7BB
��BB
��B
��B
�`B
ȴB
�LB
��B
�uB
�JB
w�B
e`B
N�B
49B
�B
B	��B	�B	�wB	�3B	��B	��B	��B	�B	y�B	ffB	bNB	\)B	[#B	\)B	W
B	G�B	?}B	6FB	.B	-B	#�B	�B	�B	uB	PB	B��B��B��B��B�B�B�`B�HB�NB�5B�5B�/B�B�B��B��BɺB��BƨBŢBB�dB�-B�B��B��B��B�7B�JB�{B�PB�7Bv�Bs�Bv�Bo�Bp�Bu�Bp�Bl�Bp�BjBffB^5BW
BZBE�BN�BT�BT�BT�BM�BN�BM�BE�B?}BQ�BS�BVBW
BXB^5BaHB]/BS�BS�BVBM�BJ�BC�BC�BC�BE�BB�B<jB@�BA�BA�BF�BE�BJ�BN�BR�BS�BQ�BT�BXB\)B]/B\)B]/BYB`BBr�Bw�Bv�Bu�Bv�Bv�Bu�By�B�B�B�B�7B�1B�7B�+B�B�DB�bB�oB�{B�uB�hB��B��B��B��B�B�B�B�B�'B�'B�3B�3B�^B�}B�wB�jB��BɺB��B��B��B��B��B��B��B�B�HB�mB�yB�B�B�B�yB�B�B�B�B�B�B�B��B��B��B��B��B	%B	1B	
=B		7B	
=B	DB	JB	PB	VB	\B	\B	hB	�B	!�B	%�B	1'B	5?B	:^B	7LB	33B	49B	49B	33B	2-B	33B	6FB	;dB	>wB	>wB	?}B	G�B	O�B	N�B	R�B	XB	YB	]/B	`BB	bNB	bNB	bNB	aHB	aHB	cTB	ffB	l�B	o�B	p�B	p�B	p�B	p�B	p�B	o�B	n�B	q�B	u�B	u�B	v�B	v�B	x�B	|�B	|�B	|�B	�B	�B	�B	�1B	�JB	�PB	�VB	�\B	�VB	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�9B	�FB	�FB	�?B	�?B	�?B	�FB	�FB	�XB	�XB	�dB	�jB	�wB	�}B	�}B	�}B	��B	��B	��B	��B	ŢB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�)B	�/B	�/B	�BB	�HB	�NB	�ZB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
  B	��B
B
B
B
B
B
B
B
B	��B	��B	��B
B
B
B
%B
+B
%B
+B
+B
	7B
	7B
	7B

=B
DB
DB
DB
DB
DB
JB
VB
VB
PB
PB
PB
\B
bB
hB
hB
hB
hB
hB
oB
oB
uB
uB
uB
uB
uB
{B
{B
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
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
$�B
%�B
%�B
%�B
%�B
%�B
&�B
'�B
(�B
'�B
&�B
&�B
&�B
'�B
(�B
)�B
)�B
)�B
,B
-B
,B
,B
,B
,B
-B
.B
.B
.B
-B
/B
0!B
/B
/B
.B
.B
/B
0!B
0!B
/B
/B
.B
,B
.B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
1'B
2-B
2-B
33B
6FB
6FB
7LB
6FB
6FB
5?B
6FB
7LB
8RB
8RB
:^B
:^B
:^B
:^B
;dB
;dB
=qB
=qB
=qB
=qB
=qB
=qB
<jB
>wB
=qB
=qB
=qB
=qB
>wB
@�B
@�B
@�B
@�B
@�B
@�B
A�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
E�B
E�B
D�B
G�B
G�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
I�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
M�B
M�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
P�B
P�B
P�B
N�B
O�B
Q�B
Q�B
R�B
S�B
S�B
S�B
S�B
T�B
S�B
T�B
S�B
T�B
VB
VB
VB
VB
VB
W
B
XB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
`BB
`BB
bNB
bNB
bNB
bNB
cTB
bNB
dZB
e`B
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
dZB
dZB
ffB
ffB
e`B
gmB
gmB
hsB
hsB
hsB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
gmB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
iyB
iyB
hsB
jB
jB
k�B
k�B
k�B
k�B
jB
jB
k�B
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
n�B
n�B
n�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
r�B
s�B
s�B
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
u�B
u�B
u�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�xB�dB�B�dB�xB�^B�^B�XB�XB�XB�fB�FB�FB�`B�nB�TB�ZB�tB��B�B��B��B�}B�
B��B��B�B7B BB)_B-]B0UB0�B-)B.BA BJ=BI�BO�BP�BH�BM�BJ�BLdBSuBS@B[#Bf�Bm�BjBhsBl�BjBd�Be`Bm�Bx�Bw2B|�B{�BwLBuZBu�By	Bu�Bw�Bu�Bm�Bl�BmwBjBhsBezB^�BVBN<BL�BD�B-�B"4B�B�B�B�B��B��B�[B�zB�iB��B��B��B��B��B��B��B|Bs�Bi*B]BJ�B1AB+kB�B^B�B �B�B
��B
�2B
�$B
̈́B
��B
�B
�2B
�B
{0B
g�B
R�B
88B
�B
�B	�B	یB	ªB	�+B	�8B	�B	�/B	��B	|�B	jB	dB	^OB	\B	\�B	W�B	J	B	A�B	8lB	0;B	.�B	%�B	 BB	B	2B	B	�B��B��B��B�xB�B��B�mB��B�BߤB�VB�OB�eB�?BЗB�VB��BˬB��B�tBÖB��B�B� B��B�BB�B��B�pB�gB�B��By�Bu�BxRBq�Br�Bv�Br�BncBq�BlBh>B`vBY�B\]BI7BP�BVBU�BU�BOvBO�BN�BG_BA�BRTBTaBV�BW�BY�B_;Ba�B^�BVBUMBW
BO�BL�BEmBD�BD�BFtBD3B>(BA�BB�BCBG�BGBK�BO�BSuBTFBR�BUgBX�B\�B]~B\�B]�BZkBa|Br�Bw�BwBvBwBwBv+Bz�B��B��B�B��B��B��B�KB��B�B��B��B�B�{B��B��B��B��B��B��B��B��B�B��B�B��B�TB��B��B��B�<B�oB�XB�PB�dB�\B�pB�}BЗB��B�
B��B�B�B�B�B�B��B��B��B�B�'B�B�B�-B�?B�B�RB�PB�HB	YB	fB	
rB		�B	
rB	�B	JB	�B	�B	.B	�B	�B	�B	!�B	%�B	0�B	5tB	:�B	7�B	4B	5%B	4�B	3�B	2�B	3�B	6zB	;dB	>wB	>�B	?�B	GzB	P.B	O\B	S&B	X_B	YKB	]dB	`vB	b�B	bhB	b�B	a�B	a�B	c�B	f�B	l�B	o�B	p�B	p�B	p�B	p�B	p�B	o�B	o B	q�B	u�B	vB	wB	w2B	y>B	}B	}"B	}VB	�AB	�mB	��B	��B	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	� B	�&B	�B	�&B	�&B	�&B	�&B	�B	�$B	�$B	�>B	�>B	�DB	�KB	�qB	�[B	�|B	�TB	�FB	�`B	�ZB	�tB	�ZB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�"B	�(B	�HB	�B	�,B	�FB	�aB	�gB	�YB	�eB	�eB	�B	�]B	�~B	ݘB	�vB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�GB	�aB	��B	��B	��B	��B	�B	�B	�0B	�B	�B	�B	�"B	�"B	�.B	�(B	�jB
 4B	�HB
 OB
 OB	�cB
;B
AB
GB
-B
aB
GB
AB
oB	�}B	��B	�cB
aB
aB
�B
?B
_B
tB
EB
_B
	lB
	�B
	�B

rB
�B
�B
xB
�B
�B
�B
pB
�B
�B
�B
�B
�B
�B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
7B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
B
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
#B
#B
#B
#B
#B
$B
$B
%,B
%�B
%�B
&B
&2B
&2B
'B
($B
)B
($B
'B
'B
'mB
(>B
)*B
*B
*0B
*eB
,=B
-B
,=B
,"B
,"B
,WB
-CB
./B
./B
.cB
-wB
/5B
0UB
/iB
/OB
.cB
.IB
/OB
0;B
0UB
/OB
/iB
.cB
,�B
.cB
0UB
0oB
0oB
1[B
1AB
1AB
1[B
1[B
2aB
1[B
2|B
2|B
3hB
6zB
6`B
7fB
6zB
6zB
5�B
6zB
7�B
8�B
8�B
:xB
:�B
:�B
:�B
;B
;�B
=�B
=�B
=qB
=�B
=�B
=�B
<�B
>�B
=�B
=�B
=�B
=�B
>�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
@�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
E�B
E�B
D�B
G�B
G�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
I�B
I�B
J	B
J	B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
MB
MB
L�B
LB
MB
MB
NB
NB
NB
N�B
N"B
N"B
O�B
O�B
PB
Q B
P�B
Q B
Q B
Q B
P�B
RB
QB
P�B
QB
Q B
O\B
P.B
R:B
R B
S&B
T,B
TB
TB
TB
U2B
TB
U2B
TFB
U2B
VB
VB
VSB
V9B
VSB
WYB
XEB
X+B
YB
Y1B
YB
Y1B
Y1B
Y1B
YKB
ZQB
Z7B
ZQB
ZQB
Z7B
Z7B
[#B
Z7B
ZQB
[qB
[WB
\CB
\CB
\CB
\CB
\CB
\CB
\]B
\CB
\]B
]dB
]dB
]IB
]IB
]IB
]~B
]~B
]~B
^jB
_VB
_VB
_pB
_pB
_VB
_pB
_�B
_�B
_�B
_�B
`vB
abB
abB
abB
abB
`vB
`vB
b�B
b�B
b�B
b�B
cnB
b�B
d�B
e`B
dZB
dtB
e`B
e`B
e`B
ezB
ezB
e�B
e�B
dtB
d�B
f�B
f�B
e�B
g�B
g�B
h�B
hsB
hsB
g�B
g�B
g�B
h�B
hsB
hsB
hsB
hsB
h�B
g�B
g�B
h�B
h�B
i�B
i�B
iyB
i�B
i�B
i�B
i�B
i�B
jB
i�B
i�B
h�B
j�B
j�B
k�B
k�B
k�B
k�B
j�B
j�B
k�B
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
n�B
n�B
n�B
o�B
o�B
o�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
r�B
s�B
s�B
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
u�B
u�B
u�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%zx<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.12(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201710200034232017102000342320171020003423201806221320372018062213203720180622132037201804050723192018040507231920180405072319  JA  ARFMdecpA19c                                                                20171016093507  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171016003530  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171016003531  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171016003532  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171016003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171016003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171016003533  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171016003533  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171016003533  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171016003533                      G�O�G�O�G�O�                JA  ARUP                                                                        20171016005652                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171016153357  CV  JULD            G�O�G�O�F�o�                JM  ARSQJMQC2.0                                                                 20171017000000  CF  PSAL_ADJUSTED_QCCz  D�� G�O�                JM  ARCAJMQC2.0                                                                 20171019153423  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171019153423  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404222319  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042037  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221515                      G�O�G�O�G�O�                