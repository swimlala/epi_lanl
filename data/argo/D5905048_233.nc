CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-04-23T00:35:45Z creation;2018-04-23T00:35:50Z conversion to V3.1;2019-12-19T07:40:04Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20180423003545  20200116221516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0577_233                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�]6��?�1   @�]7o� @4�o����dKX�e,1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D*��D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{fD{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ Dż�D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��\@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD~�D�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*��D+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9~�D9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa��DbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz��D{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�?\D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)DŸ�D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�\D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�?\D�\D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��mA��yA��HA��yA��#A�A�A���A���A���AľwAľwAľwAļjAļjAĺ^Aĺ^Aĺ^Aĺ^Aĺ^AĸRAĺ^Aĺ^Aě�AāA�z�A�p�A�O�A�=qA�"�A��HAÑhA�
=A�bNA�+A��A���A�C�A���A�K�A�$�A���A���A���A��PA�~�A�z�A�ffA�XA��PA��uA�A�A�ZA�A�A��A���A�|�A��A�ȴA�t�A�r�A�%A���A���A���A��A���A��;A�`BA�ĜA��#A�K�A��A���A�ZA��^A��A�Q�A��jA�bA�v�A�C�A���A�bNA�ZA� �A�=qA�z�A��A��+A�
=A�bNA���A�5?A�ĜA��yA��/A�;dA���A�ƨA���A�?}A� �A�$�A���A��A���A��+A�~�A�^5A�$�A��/A��A��wA��mA��`A��A���A���A�{A|�RAzM�AwoAtn�Ar(�Ao��An$�Alz�AkdZAjz�Ah�uAf�\Adr�AcoAbĜAa�A^�`A\A�AZ�HAWx�AU�AS�wAR�HAQK�AN�\AM�7AL��AK�AJ�jAI�#AF{AD��AC�AB�jAB�A@ĜA>�A=�mA<�jA;�A:�/A8�A7XA5��A4ĜA1A0��A/�FA.��A-�FA-�A-O�A,��A,  A*Q�A(�A'��A'��A'K�A&jA%G�A$1A#�PA!��A �DA��AO�A�/A��A�A^5AoA  AĜA9XA��A�DA��AA�uA�HAZAA�A=qA�A�Av�A��AVA
�A��A�AVA�/AE�A�At�A��A�+A(�A��A��AVA��A ��@�?}@��@��@�^5@�hs@�\)@�=q@��@��@�v�@�-@�^@��@�z�@��;@�33@�-@�r�@�l�@�ȴ@�-@�@��@�
=@�M�@�^@�Q�@�o@�=q@ᙚ@�S�@��@�&�@�9X@���@�M�@ف@���@� �@�ȴ@�J@Ցh@�Z@�33@�x�@ЋD@�l�@�-@���@�bN@̋D@�bN@˝�@�^5@��T@�V@���@�z�@�I�@�S�@Ɨ�@�`B@��@��/@ģ�@�I�@�|�@�o@�v�@��@��^@��h@�X@�  @�"�@��y@��@���@���@��@�Ĝ@���@�1@��D@��@��@�S�@�"�@�ȴ@��-@���@�(�@��@��@���@���@���@��`@� �@���@�+@��R@�J@�`B@��`@�bN@��m@�@���@��y@�M�@�-@�@�O�@���@��@��u@�r�@�Q�@�1@��w@�"�@�n�@��h@�7L@��7@�x�@�7L@�V@���@���@���@��u@�bN@�  @��;@���@��P@�v�@�@���@��@��#@���@��^@���@�x�@�O�@���@�1'@���@���@�t�@�C�@���@���@��@�A�@��m@�|�@�C�@�o@��y@�ȴ@��R@���@�n�@�@���@��^@���@��7@�X@�?}@�%@��@���@�bN@�b@���@��@�dZ@�+@���@�5?@�x�@��@��`@��9@��m@�|�@��@�I�@��D@�1'@� �@�ƨ@�t�@��H@��R@���@��!@��y@���@��y@��@��R@�M�@���@�x�@�X@���@��@�(�@� �@� �@�1@��;@���@��@�l�@�K�@�+@���@�ȴ@��+@��@���@�?}@�Ĝ@�z�@�I�@��@��m@��P@�
=@���@�-@��#@���@�hs@�/@��@�V@���@��j@��@�Ĝ@��`@��/@�Ĝ@���@��@�bN@�Q�@�9X@�1'@�(�@�  @�  @��m@��F@�l�@�33@�"�@�@��y@��@���@���@�E�@�E�@�E�@�$�@���@���@�/@���@���@��D@��@��9@�r�@� �@��@��
@�ƨ@��F@�l�@�S�@�@��R@�~�@�n�@�V@��@��h@��@�hs@�G�@�V@���@�Ĝ@���@��@�bN@�A�@�9X@� �@��@���@��@�\)@�C�@���@�ȴ@��!@���@�v�@�V@�{@���@���@��@�p�@�G�@�/@�&�@���@��/@��D@�Q�@�9X@��@��@~ȴ@}�h@}�@|�@|j@|(�@|1@{ƨ@{S�@z��@zn�@z-@y�7@y%@xQ�@w�@w�@v�R@u��@up�@t�@s�
@r�H@rM�@q�@qhs@p�9@o
=@n��@nV@m�T@mp�@l��@l��@l�D@l�@kƨ@k��@kdZ@kS�@kC�@k33@ko@j�!@j^5@jJ@i��@i��@i&�@h �@g|�@g+@fȴ@f5?@e�-@eO�@e?}@e/@e/@e�@d��@d�D@dj@dI�@d(�@d�@c�m@c�F@cdZ@c"�@c@b��@b��@b~�@b�@a�#@aX@a%@`�9@`b@_��@_\)@^�R@^��@^E�@]�h@]?}@\�@\�@\�D@[S�@Z�\@Z�\@Z=q@Y�#@Y��@Y��@YX@X�u@XbN@XbN@Xb@W�;@W�w@W|�@WK�@W;d@Vȴ@V$�@V{@U�@U@U��@U�@U�@T�@So@R�@R�!@R~�@RM�@R=q@Q�7@Q&�@P��@PĜ@P�u@Pr�@PQ�@O�@Ol�@Ol�@N�R@NE�@M�-@M�h@M�h@Mp�@MV@Lj@L9X@L1@K�
@KS�@Ko@J�!@Jn�@J=q@I��@IX@H��@HbN@H1'@G�;@Gl�@GK�@G;d@G
=@Fȴ@F$�@E�h@E�@D��@D��@Dz�@D�@CC�@Co@C@C@B��@Bn�@A��@A�^@AX@@�`@@bN@@ �@@  @?�;@?�@?�P@?;d@?�@?�@?
=@?
=@>�R@>��@>v�@>5?@>$�@=�T@=`B@=�@<��@<�@<(�@<1@;�F@;��@;C�@:��@:~�@:�@9��@9�@9��@9��@9��@9&�@9%@8��@8�u@8r�@8bN@8A�@8 �@8b@7�@7�;@7�@7;d@6v�@6V@6V@65?@5�@5��@5��@5O�@5�@4�@4��@4j@4Z@4Z@4(�@41@41@4�@4�@4�@3��@3��@3C�@3o@2�@2��@2��@2��@2~�@2n�@2=q@1��@1X@1&�@0Ĝ@0bN@/�@/K�@.�y@.��@.E�@-�T@-��@-�-@-p�@-O�@,�/@,�D@,�D@,9X@+��@+�m@+ƨ@+��@+��@+t�@+dZ@+33@+"�@*�@*��@*��@*n�@*J@)��@)�#@)��@)x�@)X@)G�@)&�@(��@(��@(Q�@(1'@(b@(  @'�;@'��@'��@'�P@'|�@'l�@'
=@&��@&V@&$�@&{@&@%��@%�@%`B@%O�@$��@$�/@$��@$z�@$j@$Z@$Z@$I�@$�@#�m@#��@#S�@#o@"�H@"��@"�!@"��@"�\@"�\@"n�@"M�@"-@"-@"-@"�@!��@!�^@!hs@!&�@ ��@ bN@ b@��@+@�@�@��@�@��@{@�@�@�@@p�@O�@��@��@z�@j@Z@1@C�@�@��@~�@=q@-@��@�#@��@�7@x�@x�@x�@hs@hs@X@7L@�`@��@��@��@�u@Q�@1'@  @�w@�@��@�P@\)@K�@K�@�@�R@��@��@ff@@��@@�-@p�@V@��@��@�@z�@�@��@dZ@33@o@�H@��@��@�!@�!@�\@~�@M�@M�@=q@J@�7@hs@G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��mA��yA��HA��yA��#A�A�A���A���A���AľwAľwAľwAļjAļjAĺ^Aĺ^Aĺ^Aĺ^Aĺ^AĸRAĺ^Aĺ^Aě�AāA�z�A�p�A�O�A�=qA�"�A��HAÑhA�
=A�bNA�+A��A���A�C�A���A�K�A�$�A���A���A���A��PA�~�A�z�A�ffA�XA��PA��uA�A�A�ZA�A�A��A���A�|�A��A�ȴA�t�A�r�A�%A���A���A���A��A���A��;A�`BA�ĜA��#A�K�A��A���A�ZA��^A��A�Q�A��jA�bA�v�A�C�A���A�bNA�ZA� �A�=qA�z�A��A��+A�
=A�bNA���A�5?A�ĜA��yA��/A�;dA���A�ƨA���A�?}A� �A�$�A���A��A���A��+A�~�A�^5A�$�A��/A��A��wA��mA��`A��A���A���A�{A|�RAzM�AwoAtn�Ar(�Ao��An$�Alz�AkdZAjz�Ah�uAf�\Adr�AcoAbĜAa�A^�`A\A�AZ�HAWx�AU�AS�wAR�HAQK�AN�\AM�7AL��AK�AJ�jAI�#AF{AD��AC�AB�jAB�A@ĜA>�A=�mA<�jA;�A:�/A8�A7XA5��A4ĜA1A0��A/�FA.��A-�FA-�A-O�A,��A,  A*Q�A(�A'��A'��A'K�A&jA%G�A$1A#�PA!��A �DA��AO�A�/A��A�A^5AoA  AĜA9XA��A�DA��AA�uA�HAZAA�A=qA�A�Av�A��AVA
�A��A�AVA�/AE�A�At�A��A�+A(�A��A��AVA��A ��@�?}@��@��@�^5@�hs@�\)@�=q@��@��@�v�@�-@�^@��@�z�@��;@�33@�-@�r�@�l�@�ȴ@�-@�@��@�
=@�M�@�^@�Q�@�o@�=q@ᙚ@�S�@��@�&�@�9X@���@�M�@ف@���@� �@�ȴ@�J@Ցh@�Z@�33@�x�@ЋD@�l�@�-@���@�bN@̋D@�bN@˝�@�^5@��T@�V@���@�z�@�I�@�S�@Ɨ�@�`B@��@��/@ģ�@�I�@�|�@�o@�v�@��@��^@��h@�X@�  @�"�@��y@��@���@���@��@�Ĝ@���@�1@��D@��@��@�S�@�"�@�ȴ@��-@���@�(�@��@��@���@���@���@��`@� �@���@�+@��R@�J@�`B@��`@�bN@��m@�@���@��y@�M�@�-@�@�O�@���@��@��u@�r�@�Q�@�1@��w@�"�@�n�@��h@�7L@��7@�x�@�7L@�V@���@���@���@��u@�bN@�  @��;@���@��P@�v�@�@���@��@��#@���@��^@���@�x�@�O�@���@�1'@���@���@�t�@�C�@���@���@��@�A�@��m@�|�@�C�@�o@��y@�ȴ@��R@���@�n�@�@���@��^@���@��7@�X@�?}@�%@��@���@�bN@�b@���@��@�dZ@�+@���@�5?@�x�@��@��`@��9@��m@�|�@��@�I�@��D@�1'@� �@�ƨ@�t�@��H@��R@���@��!@��y@���@��y@��@��R@�M�@���@�x�@�X@���@��@�(�@� �@� �@�1@��;@���@��@�l�@�K�@�+@���@�ȴ@��+@��@���@�?}@�Ĝ@�z�@�I�@��@��m@��P@�
=@���@�-@��#@���@�hs@�/@��@�V@���@��j@��@�Ĝ@��`@��/@�Ĝ@���@��@�bN@�Q�@�9X@�1'@�(�@�  @�  @��m@��F@�l�@�33@�"�@�@��y@��@���@���@�E�@�E�@�E�@�$�@���@���@�/@���@���@��D@��@��9@�r�@� �@��@��
@�ƨ@��F@�l�@�S�@�@��R@�~�@�n�@�V@��@��h@��@�hs@�G�@�V@���@�Ĝ@���@��@�bN@�A�@�9X@� �@��@���@��@�\)@�C�@���@�ȴ@��!@���@�v�@�V@�{@���@���@��@�p�@�G�@�/@�&�@���@��/@��D@�Q�@�9X@��@��@~ȴ@}�h@}�@|�@|j@|(�@|1@{ƨ@{S�@z��@zn�@z-@y�7@y%@xQ�@w�@w�@v�R@u��@up�@t�@s�
@r�H@rM�@q�@qhs@p�9@o
=@n��@nV@m�T@mp�@l��@l��@l�D@l�@kƨ@k��@kdZ@kS�@kC�@k33@ko@j�!@j^5@jJ@i��@i��@i&�@h �@g|�@g+@fȴ@f5?@e�-@eO�@e?}@e/@e/@e�@d��@d�D@dj@dI�@d(�@d�@c�m@c�F@cdZ@c"�@c@b��@b��@b~�@b�@a�#@aX@a%@`�9@`b@_��@_\)@^�R@^��@^E�@]�h@]?}@\�@\�@\�D@[S�@Z�\@Z�\@Z=q@Y�#@Y��@Y��@YX@X�u@XbN@XbN@Xb@W�;@W�w@W|�@WK�@W;d@Vȴ@V$�@V{@U�@U@U��@U�@U�@T�@So@R�@R�!@R~�@RM�@R=q@Q�7@Q&�@P��@PĜ@P�u@Pr�@PQ�@O�@Ol�@Ol�@N�R@NE�@M�-@M�h@M�h@Mp�@MV@Lj@L9X@L1@K�
@KS�@Ko@J�!@Jn�@J=q@I��@IX@H��@HbN@H1'@G�;@Gl�@GK�@G;d@G
=@Fȴ@F$�@E�h@E�@D��@D��@Dz�@D�@CC�@Co@C@C@B��@Bn�@A��@A�^@AX@@�`@@bN@@ �@@  @?�;@?�@?�P@?;d@?�@?�@?
=@?
=@>�R@>��@>v�@>5?@>$�@=�T@=`B@=�@<��@<�@<(�@<1@;�F@;��@;C�@:��@:~�@:�@9��@9�@9��@9��@9��@9&�@9%@8��@8�u@8r�@8bN@8A�@8 �@8b@7�@7�;@7�@7;d@6v�@6V@6V@65?@5�@5��@5��@5O�@5�@4�@4��@4j@4Z@4Z@4(�@41@41@4�@4�@4�@3��@3��@3C�@3o@2�@2��@2��@2��@2~�@2n�@2=q@1��@1X@1&�@0Ĝ@0bN@/�@/K�@.�y@.��@.E�@-�T@-��@-�-@-p�@-O�@,�/@,�D@,�D@,9X@+��@+�m@+ƨ@+��@+��@+t�@+dZ@+33@+"�@*�@*��@*��@*n�@*J@)��@)�#@)��@)x�@)X@)G�@)&�@(��@(��@(Q�@(1'@(b@(  @'�;@'��@'��@'�P@'|�@'l�@'
=@&��@&V@&$�@&{@&@%��@%�@%`B@%O�@$��@$�/@$��@$z�@$j@$Z@$Z@$I�@$�@#�m@#��@#S�@#o@"�H@"��@"�!@"��@"�\@"�\@"n�@"M�@"-@"-@"-@"�@!��@!�^@!hs@!&�@ ��@ bN@ b@��@+@�@�@��@�@��@{@�@�@�@@p�@O�@��@��@z�@j@Z@1@C�@�@��@~�@=q@-@��@�#@��@�7@x�@x�@x�@hs@hs@X@7L@�`@��@��@��@�u@Q�@1'@  @�w@�@��@�P@\)@K�@K�@�@�R@��@��@ff@@��@@�-@p�@V@��@��@�@z�@�@��@dZ@33@o@�H@��@��@�!@�!@�\@~�@M�@M�@=q@J@�7@hs@G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
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
�#B
�HB
�ZB
�yBB\B�B5?BK�B]/BgmBgmBdZBaHBaHBjBq�B�B�PB�VB�bB�uB��B�B�^BB��B��B��B��B��B��BB��B�B
=BVB�B�B�B33B2-B.BA�BE�BG�BM�B.B+B8RB2-B+B�B"�B#�B"�B&�B/B.B�B�BB��B�B�)B��B�hB�oB��B��B��B�VB{�Bw�BhsBN�B.B6FB"�B�BB
��B
��B
��B
��B
��B
�B
�sB
�)B
ÖB
�hB
y�B
s�B
ZB
O�B
C�B
$�B
oB	��B	�TB	�B	ƨB	��B	�?B	�B	��B	��B	�DB	�B	{�B	|�B	gmB	O�B	@�B	6FB	�B	�B	VB	DB	1B�B��B��B�yB�B�mBǮB�HB�#B��B�B��BÖB��BŢB�jB�}B�B��B��B��B�oB��B��B��B��B��B��B��B��B�PB�B�oB�oB�\B�7B~�B~�B|�Bv�Bs�Bu�Bn�Bw�By�Bp�BiyBgmBgmBffBl�BhsB^5BT�BXBR�BK�B_;BcTBe`BaHBW
BZBW
BVBQ�BN�BN�BVBZBW
BZBZBYB[#BZBW
BR�BR�BR�BM�BF�BS�B_;B[#BZBXBZB]/B\)BdZBgmBffBe`BgmBgmBffBdZBbNBhsBp�Bo�Bp�Br�Bs�Bv�Bx�Bu�Bt�Bw�Bv�Bp�B|�B�B�B�%B�bB�\B�oB�hB�oB��B��B�{B��B��B��B��B��B��B��B��B�B�B�B�FB�?B�dB�dB�jB�^B�dB�wBĜBȴBȴBǮBȴB��B��B�B�B��B��B��B��B��B��B��B�B�B�B�B�NB�B�B�B�B�B�B�mB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	JB	bB	uB	�B	�B	�B	!�B	%�B	)�B	+B	.B	,B	+B	(�B	+B	+B	/B	7LB	<jB	=qB	?}B	A�B	B�B	B�B	D�B	D�B	D�B	G�B	G�B	F�B	D�B	P�B	W
B	XB	YB	YB	ZB	[#B	\)B	]/B	^5B	`BB	gmB	jB	k�B	jB	hsB	hsB	q�B	r�B	x�B	z�B	~�B	�B	�B	�B	�%B	�%B	�%B	�%B	�=B	�PB	�PB	�PB	�PB	�\B	�\B	�hB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�-B	�RB	��B	ĜB	ÖB	ƨB	ŢB	ǮB	ǮB	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�B	�B	�B	�)B	�HB	�NB	�TB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
+B
+B
+B
1B
1B
+B
+B

=B

=B
	7B
1B
	7B

=B
	7B

=B
DB
PB
PB
JB
JB
PB
PB
PB
PB
PB
\B
\B
VB
bB
hB
bB
\B
hB
{B
{B
{B
uB
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
!�B
 �B
 �B
 �B
"�B
#�B
"�B
 �B
�B
"�B
#�B
#�B
$�B
$�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
'�B
(�B
(�B
(�B
)�B
'�B
&�B
'�B
)�B
)�B
)�B
+B
,B
-B
.B
.B
-B
-B
-B
.B
.B
.B
.B
.B
.B
/B
0!B
0!B
1'B
0!B
1'B
0!B
0!B
0!B
0!B
0!B
/B
1'B
0!B
0!B
2-B
2-B
1'B
33B
33B
33B
33B
1'B
2-B
6FB
5?B
5?B
5?B
5?B
49B
33B
49B
5?B
49B
49B
5?B
5?B
6FB
6FB
5?B
5?B
7LB
6FB
6FB
6FB
6FB
49B
33B
33B
7LB
8RB
8RB
8RB
8RB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
8RB
:^B
;dB
:^B
<jB
<jB
>wB
?}B
>wB
>wB
>wB
@�B
@�B
?}B
?}B
@�B
@�B
A�B
A�B
@�B
?}B
?}B
@�B
A�B
A�B
A�B
C�B
C�B
B�B
A�B
A�B
A�B
C�B
C�B
E�B
E�B
D�B
C�B
F�B
F�B
F�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
H�B
I�B
I�B
I�B
I�B
H�B
H�B
I�B
J�B
J�B
I�B
K�B
J�B
J�B
J�B
J�B
K�B
K�B
L�B
M�B
M�B
M�B
L�B
L�B
M�B
M�B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
M�B
M�B
M�B
O�B
P�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
R�B
S�B
R�B
R�B
Q�B
R�B
S�B
R�B
R�B
R�B
T�B
T�B
T�B
VB
VB
XB
XB
W
B
XB
W
B
XB
YB
XB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
ZB
ZB
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
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
aHB
bNB
aHB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
ffB
ffB
ffB
e`B
e`B
e`B
ffB
gmB
gmB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
gmB
e`B
gmB
hsB
hsB
hsB
iyB
iyB
jB
jB
jB
jB
k�B
k�B
jB
jB
jB
jB
iyB
k�B
k�B
jB
k�B
jB
k�B
k�B
k�B
l�B
l�B
k�B
k�B
l�B
l�B
k�B
k�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
m�B
m�B
o�B
o�B
n�B
n�B
n�B
n�B
o�B
p�B
q�B
p�B
q�B
r�B
r�B
r�B
q�B
q�B
q�B
r�B
r�B
q�B
q�B
s�B
s�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
��B
��B
��B
�B
��B
��B
��B
��B
� B
� B
��B
��B
��B
� B
��B
�B
��B
�B
��B
�B
�&B
�@B
�qB
�|B
�B
��BGB�BdB6+BMB^jBg�Bg�Be,Bb�Bb�BkQBrGB�mB��B�pB�}B��B��B�/B�xB�[B�B��B�oB��B��B��B�B �B�B6BHB�B�B�B33B2�B0oBC�BIBK)BP�B2�B-]B9	B3hB,�B!�B$&B%,B$@B(>B/�B/�B �BB�B�B��B��B�B��B�9B�yB�vB��B�}B~BBy�Bj�BSB2aB8B%zBB�B
�HB
�B
�`B
��B
�?B
�UB
�_B
�~B
��B
��B
~BB
v�B
^�B
RB
F?B
(�B
�B	��B	�B	��B	ɠB	�uB	�LB	��B	��B	�!B	��B	��B	}�B	}�B	i�B	R�B	C�B	8�B	�B	�B	�B	�B	
rB��B�.B�B�B�B�*B�B��BܒBҽB�7B͹B�B��B�EB�B��B��B�B�B��B��B�!B�;B�B��B�:B�hB��B��B��B�mB�&B�&B�.B��B��B��B~(By	Bu�Bw�Bp�Bx�BzxBr-Bk�BiDBh�Bh
BmwBi�B_�BW?BY�BT�BM�B_�Bc�Be�Ba�BX�B[	BX+BWYBS�BP�BPHBV�BZ�BW�BZ�BZ�BY�B[�BZ�BXBS�BTBT,BOvBI7BT�B_�B\B[#BYeB[#B^OB]/Bd�Bg�Bf�BfBg�Bh
BgBe`BcnBiDBq'Bp�Bq[BshBt�BwfBy�Bv�Bu�Bx�Bw�Br-B}�B��B�B�B��B�B�B� B�uB�EB�=B��B��B��B�jB��B��B��B�-B��B�]B��B��B��B��B��B��B��B�0B�B�.B��B��B�B�1B�lB�<B�hB�SB�SB�gB�gB��B҉B�gB҉B�[B�SBևB�B��B�4B�B�B�B��B��B�"B�>B�0B��B��B�B�B�5B��B�[B�TB�8B�XB�DB�rB�B	 iB	�B	�B	�B	�B	}B	�B	�B		B	#B	"4B	&2B	*0B	+QB	.IB	,WB	+kB	)�B	+�B	+�B	/iB	7LB	<�B	=�B	?�B	A�B	B�B	B�B	D�B	D�B	EB	G�B	G�B	G+B	ESB	Q4B	W
B	XEB	Y1B	Y1B	ZQB	[=B	\xB	]~B	^�B	`�B	g�B	j�B	k�B	j�B	iB	iDB	rB	sMB	y>B	{JB	.B	�UB	�GB	�3B	�YB	�YB	�tB	��B	�XB	��B	��B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�VB	�8B	�=B	�kB	��B	�|B	�RB	� B	āB	��B	��B	��B	��B	�1B	�B	��B	��B	��B	��B	�B	�B	�SB	�uB	�aB	�2B	�SB	ևB	ևB	�CB	�bB	�hB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	� B	��B	��B	��B	�B	��B	�B	�B	�B	�-B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�(B	�.B	�.B
 B
 4B
;B
AB
AB
AB
GB
MB
gB
mB
EB
EB
EB
fB
KB
zB
zB

=B

rB
	�B
�B
	lB

�B
	lB

rB
xB
jB
�B
�B
�B
jB
�B
�B
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
B
�B
�B
�B
�B
�B
 B
�B
 �B
 'B
 B
 B
 'B
 �B
"B
 'B
"B
!-B
!-B
!B
#B
$&B
# B
!B
VB
#B
#�B
$&B
%,B
%B
'B
'B
'8B
'B
(
B
)*B
)B
)B
)*B
)*B
(>B
)*B
)B
)B
*B
(XB
'RB
(>B
*B
*0B
*KB
+6B
,=B
-)B
./B
.B
-)B
-CB
-)B
.IB
.IB
./B
./B
.IB
.IB
/OB
0UB
0UB
1AB
0UB
1[B
0UB
0UB
0oB
0UB
0UB
/iB
1AB
0oB
0oB
2aB
2aB
1vB
3hB
3MB
3MB
3hB
1�B
2|B
6FB
5ZB
5tB
5ZB
5tB
4nB
3�B
4TB
5ZB
4nB
4nB
5ZB
5tB
6zB
6zB
5�B
5�B
7fB
6zB
6zB
6zB
6`B
4nB
3�B
3�B
7fB
8lB
8�B
8�B
8�B
7�B
7fB
8lB
8lB
9�B
9�B
9rB
8�B
:�B
;B
:�B
<�B
<�B
>�B
?�B
>�B
>�B
>�B
@�B
@�B
?�B
?�B
@�B
@�B
A�B
A�B
@�B
?�B
?�B
@�B
A�B
A�B
A�B
C�B
C�B
B�B
A�B
A�B
A�B
C�B
C�B
E�B
E�B
D�B
C�B
F�B
F�B
F�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
H�B
I�B
I�B
I�B
I�B
H�B
H�B
I�B
J�B
J�B
J	B
K�B
J�B
J�B
J�B
KB
K�B
K�B
L�B
M�B
NB
M�B
MB
MB
NB
NB
MB
NB
N�B
N�B
N�B
N�B
N�B
OB
M�B
NB
NB
O�B
Q B
PB
PB
PB
PB
O�B
Q B
Q B
Q B
QB
RB
Q�B
R B
RB
R�B
SB
SB
SB
R B
R B
R B
S&B
S&B
S&B
TB
S&B
TB
SB
S&B
R B
S@B
T,B
S@B
S&B
S[B
UB
UMB
U2B
VB
V9B
XB
XEB
W?B
XEB
WYB
X+B
Y1B
XEB
YKB
Z7B
Z7B
Z7B
Z7B
Z7B
ZQB
Z7B
Z7B
ZQB
ZQB
[WB
ZQB
Z7B
\CB
\]B
\]B
\]B
]IB
]dB
]dB
]dB
]dB
]IB
^jB
^OB
_;B
_pB
_pB
_pB
_VB
_VB
_pB
^�B
^�B
_pB
`\B
`BB
`vB
`vB
a|B
a|B
abB
`vB
abB
a|B
bNB
bhB
bNB
bhB
bhB
abB
bhB
a|B
b�B
b�B
cnB
cnB
dZB
dtB
dtB
dZB
dtB
dtB
dtB
dZB
dtB
dtB
c�B
c�B
c�B
cnB
cnB
c�B
d�B
d�B
d�B
f�B
ffB
f�B
e�B
ezB
e�B
f�B
g�B
g�B
f�B
f�B
g�B
g�B
g�B
h�B
h�B
h�B
g�B
e�B
g�B
h�B
h�B
h�B
i�B
i�B
j�B
j�B
j�B
jB
k�B
k�B
j�B
jB
jB
j�B
i�B
k�B
k�B
j�B
k�B
j�B
k�B
k�B
k�B
l�B
l�B
k�B
k�B
l�B
l�B
k�B
k�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
m�B
m�B
o�B
o�B
n�B
n�B
n�B
n�B
o�B
p�B
q�B
p�B
q�B
r�B
r�B
r�B
q�B
q�B
q�B
r�B
r�B
q�B
q�B
s�B
s�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.12(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201804270034532018042700345320180427003453201806221329292018062213292920180622132929201804261708072018042617080720180426170807  JA  ARFMdecpA19c                                                                20180423093519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180423003545  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180423003548  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180423003548  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180423003549  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180423003549  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180423003549  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180423003549  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180423003550  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180423003550                      G�O�G�O�G�O�                JA  ARUP                                                                        20180423005715                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180423153315  CV  JULD            G�O�G�O�F��                JM  ARSQOW  1.1 2017V1                                                          20180426080807  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180426153453  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180426153453  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622042929  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116221516                      G�O�G�O�G�O�                