CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-07-18T09:00:48Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �<   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݌   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20210718090048  20210718090048  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               pA   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @ل�hY�1   @ل�p@;���Q��c��$�/1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         pA   A   F   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�C3Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��\@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HCǮC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX��DY~�DY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��\D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�8�D�x�D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D���D�8�D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�?\D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D��D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��\D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D�D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aˣ�A˩�AˬAˮA˰!A˰!A˰!A˲-A˲-A˰!A˰!A˴9A˼jA˶FA˺^A˩�Aˇ+A�\)A��A���A�t�A���A��A�A�A�bA�oA���A��yA�JA�VA�JA���A�p�A�=qA���A���A���A��A�+A�A�bA��^A�/A�jA�A��A�G�A�G�A��#A�O�A��A�A�A��mA���A�A�A��wA�l�A�;dA���A�  A�z�A��A��A�5?A�JA�{A��wA�l�A�  A���A���A�l�A�A�jA�C�A��A��uA�^5A��A���A�|�A���A��A���A��DA�v�A���A�ĜA�&�A�VA�I�A��A��#A���A��A�;dA��A�1A�(�A�A�A��A�AƨA~��A}��A|E�Az �Ay�7Aw��Av��Au��AuVAt  ApVAn��AljAkhsAj�HAj~�Ai��Ai7LAg|�AcK�A`��A_��A^z�A]�7A]A\��A[��A[33AZE�AY"�AX-AW��AV$�AU��ATA�AS��ARȴAQƨAQG�APbNAN��ANA�AM��AL�uAKp�AH�jAF��ADȴAD{AC/ABr�AA;dA@5?A?�A>�A<��A;�A;�A;C�A:�RA:{A9�-A9�A8��A8(�A7�A7VA6{A5�A5x�A4�`A4A3oA2JA1\)A0VA/��A.��A-��A-��A-\)A,5?A+��A*��A)��A(bNA'S�A&��A%;dA#C�A"r�A"1'A!�mA!\)A ��A��A��Az�A  A��AE�At�A�A^5AI�AI�AE�AE�A=qAVA
=A=qA\)AoA�jAffA �AVA�mA+A��AdZA�DA+A
�+A
n�A
bNA
M�A	��A�A`BAz�A��A��A1'A�A&�A�DAM�A�@��@���@��#@�1'@��h@���@�E�@��/@��
@�ȴ@��T@�F@��@�ȴ@䛦@�(�@�R@��@߮@ݺ^@�hs@���@���@�ƨ@��@���@�$�@�x�@��@ؓu@�z�@ם�@�$�@�7L@���@ѩ�@���@϶F@�G�@��;@�t�@�ff@�@��`@�I�@Ǖ�@�33@���@�5?@���@��/@��@�hs@���@���@���@���@��@�Z@��m@��@��@��-@��@���@�E�@��@�x�@�G�@��@�  @�V@��-@��`@�I�@���@�X@���@�A�@��@��
@���@�p�@�&�@��/@���@�S�@���@��@�x�@�/@�%@���@�A�@��;@�\)@��@�5?@��-@��-@�x�@�V@�9X@��@��H@��T@�7L@��@�Q�@�b@�ƨ@�~�@���@���@�p�@�7L@��@�z�@�b@��@��w@�33@�~�@�5?@���@�?}@��@��@���@�j@�9X@� �@���@���@�dZ@�;d@�"�@�
=@��H@��!@�M�@��#@��7@�/@��9@�Z@� �@��P@�t�@�dZ@�\)@�
=@���@��@��T@��@�O�@�%@���@�Ĝ@���@�Z@�1@��m@�dZ@�S�@��y@�ff@�J@�@���@���@���@��h@�`B@���@�z�@�1'@�1@��
@��w@���@�l�@��@�ȴ@��R@���@�^5@�-@�{@�@��@�@�?}@�&�@��@�Ĝ@��@�bN@��@���@�t�@�K�@�33@���@��!@���@�n�@�5?@��@��#@�@��-@��@�&�@��/@��u@�Z@l�@l�@�@~��@~v�@~ff@~V@}�@}�@|��@|��@|j@|�@|(�@|1@zn�@zM�@z-@y�#@y��@y%@w��@w�@vȴ@v�+@vV@u�@up�@uV@t�/@t�/@t��@t9X@tI�@tZ@t��@u�@tz�@s�@s33@r�!@s"�@tz�@tj@s��@r~�@p  @o�P@n��@m�@m�T@m�T@m�h@mO�@m/@m/@m/@m?}@m`B@m/@lz�@l�@kC�@j��@jM�@iX@h�9@hb@g�@g�P@g�@f�R@f�+@fV@fV@f{@e�h@e?}@d��@d�/@dZ@c��@cƨ@ct�@c@b��@b^5@a��@a��@a�7@`��@`��@`�@`A�@` �@_�w@_|�@_;d@^��@^v�@^ff@^V@^E�@^$�@]�@]�-@]O�@\��@\�@\��@\��@\�@\�D@\�@[�m@[ƨ@[�F@[��@[dZ@[C�@Z��@Y��@Y�^@Y�7@Y&�@Yhs@YX@W�P@W;d@V��@Vȴ@V��@VE�@V{@U��@U�h@U�@U�@U��@U�T@V$�@U��@UV@Tz�@T(�@S��@S�
@S�F@S�@SS�@S33@R��@R^5@RJ@Q��@Qhs@Q7L@Q%@P�`@P��@P��@P�@Pr�@PA�@O�@O�@Ol�@O;d@O+@O�@N��@N�R@Nff@N{@M�@M�h@M`B@L�D@LI�@K�
@K��@KdZ@KC�@K"�@J^5@Ihs@H�`@H1'@G�@G�P@G\)@G+@F�R@F��@F��@F�+@F{@E�@E@E�h@E`B@E?}@E�@D��@D�@D�j@D9X@C�
@Ct�@CC�@C@B��@Bn�@BM�@B=q@B�@A�#@A��@A7L@@�9@@bN@?�;@?�w@?��@?��@?�P@?|�@?\)@?
=@>�R@>�R@>�R@>V@>$�@=�T@=`B@<��@<�D@<9X@;�F@;S�@;o@:�H@:��@:�\@:�\@:�\@:�\@:^5@:-@:�@9��@9�^@9X@9�@8��@8��@8Q�@7�;@7�w@7��@7\)@7\)@7+@6�y@6�+@6E�@5��@5O�@4�@4z�@4Z@4(�@3�F@3��@3C�@2�@2��@2��@2��@2�!@2��@2�\@1�#@1�7@1G�@1%@0��@01'@01'@0 �@/�;@/\)@/�@/
=@/
=@.��@.ȴ@.�R@.�+@.$�@-@-�h@-p�@-�@,��@,j@,9X@+�
@+��@+t�@+dZ@+C�@+o@*��@*n�@*=q@*�@*�@*J@)��@)x�@)&�@)�@)�@)�@)�@)�@)%@(��@(�@(1'@'�@'��@'�w@'�@'��@'|�@'|�@'\)@'K�@&�@&�R@&��@&v�@&E�@%�T@%�h@%O�@$�/@$��@$�j@$�@$�D@$I�@#��@#�
@#�F@#��@#"�@"�\@"n�@"M�@!��@!�7@!7L@!%@ ��@ �`@ �9@ �@  �@�w@��@|�@l�@l�@\)@\)@;d@+@+@+@+@�@��@ȴ@E�@��@��@p�@O�@?}@�@��@�/@�j@��@j@I�@��@�
@�F@S�@@��@�\@~�@n�@M�@J@��@��@�7@x�@hs@7L@�`@r�@ �@�w@��@l�@;d@
=@�y@��@v�@V@�@�T@��@��@?}@/@�@�@�/@��@�@z�@9X@�@1@�m@ƨ@��@�@t�@t�@dZ@S�@33@"�@�@�!@��@~�@~�@n�@�@��@��@��@x�@hs@hs@X@G�@&�@�`@�u@�@Q�@1'@��@�w@�P@l�@K�@�@�@�R@��@�+@V@5?@{@@�@p�@`B@O�@/@��@�j@�D@j@I�@1@�
@�F@��@�@t�@S�@33@"�@o@
�@
��@
��@
^5@
=q@
=q@
=q@
J@	�@	��@	�7@	x�@	X@	X@	7L@	&�@	�@	%@	%@�`@Ĝ@�9@�@1'@b@  @�@�;@�;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aˣ�A˩�AˬAˮA˰!A˰!A˰!A˲-A˲-A˰!A˰!A˴9A˼jA˶FA˺^A˩�Aˇ+A�\)A��A���A�t�A���A��A�A�A�bA�oA���A��yA�JA�VA�JA���A�p�A�=qA���A���A���A��A�+A�A�bA��^A�/A�jA�A��A�G�A�G�A��#A�O�A��A�A�A��mA���A�A�A��wA�l�A�;dA���A�  A�z�A��A��A�5?A�JA�{A��wA�l�A�  A���A���A�l�A�A�jA�C�A��A��uA�^5A��A���A�|�A���A��A���A��DA�v�A���A�ĜA�&�A�VA�I�A��A��#A���A��A�;dA��A�1A�(�A�A�A��A�AƨA~��A}��A|E�Az �Ay�7Aw��Av��Au��AuVAt  ApVAn��AljAkhsAj�HAj~�Ai��Ai7LAg|�AcK�A`��A_��A^z�A]�7A]A\��A[��A[33AZE�AY"�AX-AW��AV$�AU��ATA�AS��ARȴAQƨAQG�APbNAN��ANA�AM��AL�uAKp�AH�jAF��ADȴAD{AC/ABr�AA;dA@5?A?�A>�A<��A;�A;�A;C�A:�RA:{A9�-A9�A8��A8(�A7�A7VA6{A5�A5x�A4�`A4A3oA2JA1\)A0VA/��A.��A-��A-��A-\)A,5?A+��A*��A)��A(bNA'S�A&��A%;dA#C�A"r�A"1'A!�mA!\)A ��A��A��Az�A  A��AE�At�A�A^5AI�AI�AE�AE�A=qAVA
=A=qA\)AoA�jAffA �AVA�mA+A��AdZA�DA+A
�+A
n�A
bNA
M�A	��A�A`BAz�A��A��A1'A�A&�A�DAM�A�@��@���@��#@�1'@��h@���@�E�@��/@��
@�ȴ@��T@�F@��@�ȴ@䛦@�(�@�R@��@߮@ݺ^@�hs@���@���@�ƨ@��@���@�$�@�x�@��@ؓu@�z�@ם�@�$�@�7L@���@ѩ�@���@϶F@�G�@��;@�t�@�ff@�@��`@�I�@Ǖ�@�33@���@�5?@���@��/@��@�hs@���@���@���@���@��@�Z@��m@��@��@��-@��@���@�E�@��@�x�@�G�@��@�  @�V@��-@��`@�I�@���@�X@���@�A�@��@��
@���@�p�@�&�@��/@���@�S�@���@��@�x�@�/@�%@���@�A�@��;@�\)@��@�5?@��-@��-@�x�@�V@�9X@��@��H@��T@�7L@��@�Q�@�b@�ƨ@�~�@���@���@�p�@�7L@��@�z�@�b@��@��w@�33@�~�@�5?@���@�?}@��@��@���@�j@�9X@� �@���@���@�dZ@�;d@�"�@�
=@��H@��!@�M�@��#@��7@�/@��9@�Z@� �@��P@�t�@�dZ@�\)@�
=@���@��@��T@��@�O�@�%@���@�Ĝ@���@�Z@�1@��m@�dZ@�S�@��y@�ff@�J@�@���@���@���@��h@�`B@���@�z�@�1'@�1@��
@��w@���@�l�@��@�ȴ@��R@���@�^5@�-@�{@�@��@�@�?}@�&�@��@�Ĝ@��@�bN@��@���@�t�@�K�@�33@���@��!@���@�n�@�5?@��@��#@�@��-@��@�&�@��/@��u@�Z@l�@l�@�@~��@~v�@~ff@~V@}�@}�@|��@|��@|j@|�@|(�@|1@zn�@zM�@z-@y�#@y��@y%@w��@w�@vȴ@v�+@vV@u�@up�@uV@t�/@t�/@t��@t9X@tI�@tZ@t��@u�@tz�@s�@s33@r�!@s"�@tz�@tj@s��@r~�@p  @o�P@n��@m�@m�T@m�T@m�h@mO�@m/@m/@m/@m?}@m`B@m/@lz�@l�@kC�@j��@jM�@iX@h�9@hb@g�@g�P@g�@f�R@f�+@fV@fV@f{@e�h@e?}@d��@d�/@dZ@c��@cƨ@ct�@c@b��@b^5@a��@a��@a�7@`��@`��@`�@`A�@` �@_�w@_|�@_;d@^��@^v�@^ff@^V@^E�@^$�@]�@]�-@]O�@\��@\�@\��@\��@\�@\�D@\�@[�m@[ƨ@[�F@[��@[dZ@[C�@Z��@Y��@Y�^@Y�7@Y&�@Yhs@YX@W�P@W;d@V��@Vȴ@V��@VE�@V{@U��@U�h@U�@U�@U��@U�T@V$�@U��@UV@Tz�@T(�@S��@S�
@S�F@S�@SS�@S33@R��@R^5@RJ@Q��@Qhs@Q7L@Q%@P�`@P��@P��@P�@Pr�@PA�@O�@O�@Ol�@O;d@O+@O�@N��@N�R@Nff@N{@M�@M�h@M`B@L�D@LI�@K�
@K��@KdZ@KC�@K"�@J^5@Ihs@H�`@H1'@G�@G�P@G\)@G+@F�R@F��@F��@F�+@F{@E�@E@E�h@E`B@E?}@E�@D��@D�@D�j@D9X@C�
@Ct�@CC�@C@B��@Bn�@BM�@B=q@B�@A�#@A��@A7L@@�9@@bN@?�;@?�w@?��@?��@?�P@?|�@?\)@?
=@>�R@>�R@>�R@>V@>$�@=�T@=`B@<��@<�D@<9X@;�F@;S�@;o@:�H@:��@:�\@:�\@:�\@:�\@:^5@:-@:�@9��@9�^@9X@9�@8��@8��@8Q�@7�;@7�w@7��@7\)@7\)@7+@6�y@6�+@6E�@5��@5O�@4�@4z�@4Z@4(�@3�F@3��@3C�@2�@2��@2��@2��@2�!@2��@2�\@1�#@1�7@1G�@1%@0��@01'@01'@0 �@/�;@/\)@/�@/
=@/
=@.��@.ȴ@.�R@.�+@.$�@-@-�h@-p�@-�@,��@,j@,9X@+�
@+��@+t�@+dZ@+C�@+o@*��@*n�@*=q@*�@*�@*J@)��@)x�@)&�@)�@)�@)�@)�@)�@)%@(��@(�@(1'@'�@'��@'�w@'�@'��@'|�@'|�@'\)@'K�@&�@&�R@&��@&v�@&E�@%�T@%�h@%O�@$�/@$��@$�j@$�@$�D@$I�@#��@#�
@#�F@#��@#"�@"�\@"n�@"M�@!��@!�7@!7L@!%@ ��@ �`@ �9@ �@  �@�w@��@|�@l�@l�@\)@\)@;d@+@+@+@+@�@��@ȴ@E�@��@��@p�@O�@?}@�@��@�/@�j@��@j@I�@��@�
@�F@S�@@��@�\@~�@n�@M�@J@��@��@�7@x�@hs@7L@�`@r�@ �@�w@��@l�@;d@
=@�y@��@v�@V@�@�T@��@��@?}@/@�@�@�/@��@�@z�@9X@�@1@�m@ƨ@��@�@t�@t�@dZ@S�@33@"�@�@�!@��@~�@~�@n�@�@��@��@��@x�@hs@hs@X@G�@&�@�`@�u@�@Q�@1'@��@�w@�P@l�@K�@�@�@�R@��@�+@V@5?@{@@�@p�@`B@O�@/@��@�j@�D@j@I�@1@�
@�F@��@�@t�@S�@33@"�@o@
�@
��@
��@
^5@
=q@
=q@
=q@
J@	�@	��@	�7@	x�@	X@	X@	7L@	&�@	�@	%@	%@�`@Ĝ@�9@�@1'@b@  @�@�;@�;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�9B�9B�3B�3B�-B�-B�-B�'B�'B�'B�'B�'B�!B�!B�B�B�B��B�7BJ�B@�B<jB=qB=qB;dB=qB?}BB�BA�BA�BH�BH�BP�BQ�BN�BR�BT�BW
BW
BR�BS�BQ�BN�BI�BF�BE�BB�BA�B@�B>wB9XB33B/B1'B0!B-B&�B"�B�BuB
=BB��B��B��B��B��B�B�B�mB�;B�B�)B�B�B��B��B�XB�'B�B��B�{By�B]/B5?B"�BbBB�B��B�dB�B��B��B��B{�Bk�BZBC�B33B�BbB��B�B�B�;B��BɺB�}B�FB�'B�B��B�uB�B~�B}�By�Bv�Bs�Bn�BhsBN�B=qB7LB6FB33B1'B1'B.B(�B"�B�B�B�BJB%BBB  B
��B
��B
�B
�B
�B
�B
�mB
�ZB
�#B
��B
ɺB
ǮB
ÖB
��B
�dB
�?B
�'B
�B
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
�{B
�bB
�\B
�PB
�DB
�1B
�B
�B
|�B
z�B
v�B
u�B
q�B
p�B
o�B
l�B
iyB
e`B
cTB
_;B
\)B
XB
S�B
L�B
H�B
F�B
D�B
A�B
?}B
=qB
9XB
7LB
6FB
33B
1'B
-B
'�B
'�B
'�B
'�B
&�B
&�B
$�B
#�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
hB
\B
VB
DB

=B
1B
1B
+B
%B
%B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�sB	�mB	�`B	�`B	�TB	�NB	�NB	�BB	�BB	�/B	�)B	�/B	�#B	�)B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�)B	�)B	�)B	�)B	�)B	�)B	�5B	�5B	�5B	�5B	�/B	�;B	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
+B

=B
DB
DB
JB
hB
�B
�B
�B
�B
�B
�B
#�B
%�B
)�B
)�B
+B
.B
.B
/B
1'B
33B
49B
:^B
<jB
=qB
A�B
C�B
D�B
E�B
I�B
K�B
K�B
N�B
Q�B
W
B
YB
ZB
[#B
^5B
_;B
_;B
aHB
aHB
cTB
ffB
k�B
o�B
r�B
t�B
t�B
u�B
u�B
z�B
|�B
}�B
�B
�B
�B
�B
�%B
�1B
�=B
�DB
�\B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�'B
�FB
�LB
�^B
�jB
�wB
�}B
��B
��B
B
ĜB
ĜB
ƨB
ǮB
ȴB
ɺB
��B
��B
��B
��B
��B
��B
�B
�#B
�;B
�BB
�HB
�NB
�TB
�ZB
�sB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��BBBBB1B
=BDBVB\BbBoB{B�B�B�B�B �B!�B"�B#�B#�B&�B(�B+B1'B49B6FB7LB8RB8RB8RB:^B?}B?}B@�BA�BD�BE�BF�BH�BI�BK�BL�BL�BM�BQ�BR�BS�BVBVBW
BYB\)B]/B^5B`BBdZBdZBe`BffBm�Bm�Bl�Bl�Bn�Br�Bw�Bx�Bx�Bw�Bw�Bw�Bv�Bx�By�B{�B|�B}�B~�B� B� B�B�B�%B�%B�%B�%B�%B�+B�7B�7B�JB�PB�PB�\B�bB�hB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�-B�9B�?B�FB�FB�LB�XB�dB�jB�qB�wB�wB�}B��B��B��B��B��B��B��B��B��BBÖBÖBÖBĜBŢBǮBƨBƨBǮBǮBȴBȴBɺBɺB��B��B��B��B��B��B��B�
B�B�B�#B�#B�#B�)B�/B�/B�5B�;B�;B�;B�BB�HB�HB�HB�NB�NB�NB�ZB�ZB�`B�`B�fB�fB�fB�mB�mB�sB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBBBBBB%B+B+B1B	7B	7B	7B
=B
=B
=B
=B
=BDBDBDBDBJBPBVBVB\B\BbBbBhBhBhBoBoBuBuB{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B!�B"�B"�B"�B"�B#�B#�B$�B$�B$�B$�B%�B%�B%�B&�B&�B&�B&�B&�B&�B&�B&�B'�B'�B(�B(�B(�B(�B(�B)�B)�B)�B)�B+B+B+B+B,B,B,B-B.B.B.B.B.B/B/B/B/B/B0!B1'B1'B1'B2-B2-B33B33B33B33B49B49B5?B5?B5?B6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB7LB7LB8RB8RB9XB9XB9XB9XB9XB:^B:^B:^B:^B:^B;dB;dB;dB<jB<jB=qB=qB=qB=qB=qB>wB>wB>wB>wB>wB>wB?}B?}B@�B@�BA�BA�BA�BA�BB�BB�BB�BB�BC�BC�BC�BC�BD�BD�BE�BE�BE�BE�BE�BE�BF�BF�BF�BF�BG�BG�BG�BG�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BI�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BK�BK�BL�BL�BL�BL�BM�BM�BM�BN�BN�BN�BN�BO�BO�BO�BO�BO�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BQ�BR�BR�BR�BS�BS�BS�BS�BT�BT�BT�BT�BT�BT�BT�BVBVBVBW
BW
BW
BW
BW
BW
BXBXBXBXBXBXBXBYBYBYBYBYBYBYBZBZBZBZB[#B[#444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B�9B�9B�3B�3B�-B�-B�-B�'B�'B�'B�'B�'B�!B�!B�B�B�B��B�7BJ�B@�B<jB=qB=qB;dB=qB?}BB�BA�BA�BH�BH�BP�BQ�BN�BR�BT�BW
BW
BR�BS�BQ�BN�BI�BF�BE�BB�BA�B@�B>wB9XB33B/B1'B0!B-B&�B"�B�BuB
=BB��B��B��B��B��B�B�B�mB�;B�B�)B�B�B��B��B�XB�'B�B��B�{By�B]/B5?B"�BbBB�B��B�dB�B��B��B��B{�Bk�BZBC�B33B�BbB��B�B�B�;B��BɺB�}B�FB�'B�B��B�uB�B~�B}�By�Bv�Bs�Bn�BhsBN�B=qB7LB6FB33B1'B1'B.B(�B"�B�B�B�BJB%BBB  B
��B
��B
�B
�B
�B
�B
�mB
�ZB
�#B
��B
ɺB
ǮB
ÖB
��B
�dB
�?B
�'B
�B
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
�{B
�bB
�\B
�PB
�DB
�1B
�B
�B
|�B
z�B
v�B
u�B
q�B
p�B
o�B
l�B
iyB
e`B
cTB
_;B
\)B
XB
S�B
L�B
H�B
F�B
D�B
A�B
?}B
=qB
9XB
7LB
6FB
33B
1'B
-B
'�B
'�B
'�B
'�B
&�B
&�B
$�B
#�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
hB
\B
VB
DB

=B
1B
1B
+B
%B
%B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�sB	�mB	�`B	�`B	�TB	�NB	�NB	�BB	�BB	�/B	�)B	�/B	�#B	�)B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�)B	�)B	�)B	�)B	�)B	�)B	�5B	�5B	�5B	�5B	�/B	�;B	�HB	�NB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
B
+B

=B
DB
DB
JB
hB
�B
�B
�B
�B
�B
�B
#�B
%�B
)�B
)�B
+B
.B
.B
/B
1'B
33B
49B
:^B
<jB
=qB
A�B
C�B
D�B
E�B
I�B
K�B
K�B
N�B
Q�B
W
B
YB
ZB
[#B
^5B
_;B
_;B
aHB
aHB
cTB
ffB
k�B
o�B
r�B
t�B
t�B
u�B
u�B
z�B
|�B
}�B
�B
�B
�B
�B
�%B
�1B
�=B
�DB
�\B
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�'B
�FB
�LB
�^B
�jB
�wB
�}B
��B
��B
B
ĜB
ĜB
ƨB
ǮB
ȴB
ɺB
��B
��B
��B
��B
��B
��B
�B
�#B
�;B
�BB
�HB
�NB
�TB
�ZB
�sB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��BBBBB1B
=BDBVB\BbBoB{B�B�B�B�B �B!�B"�B#�B#�B&�B(�B+B1'B49B6FB7LB8RB8RB8RB:^B?}B?}B@�BA�BD�BE�BF�BH�BI�BK�BL�BL�BM�BQ�BR�BS�BVBVBW
BYB\)B]/B^5B`BBdZBdZBe`BffBm�Bm�Bl�Bl�Bn�Br�Bw�Bx�Bx�Bw�Bw�Bw�Bv�Bx�By�B{�B|�B}�B~�B� B� B�B�B�%B�%B�%B�%B�%B�+B�7B�7B�JB�PB�PB�\B�bB�hB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�-B�9B�?B�FB�FB�LB�XB�dB�jB�qB�wB�wB�}B��B��B��B��B��B��B��B��B��BBÖBÖBÖBĜBŢBǮBƨBƨBǮBǮBȴBȴBɺBɺB��B��B��B��B��B��B��B�
B�B�B�#B�#B�#B�)B�/B�/B�5B�;B�;B�;B�BB�HB�HB�HB�NB�NB�NB�ZB�ZB�`B�`B�fB�fB�fB�mB�mB�sB�sB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BBBBBBBBBBBBBBBB%B+B+B1B	7B	7B	7B
=B
=B
=B
=B
=BDBDBDBDBJBPBVBVB\B\BbBbBhBhBhBoBoBuBuB{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B!�B!�B!�B"�B"�B"�B"�B#�B#�B$�B$�B$�B$�B%�B%�B%�B&�B&�B&�B&�B&�B&�B&�B&�B'�B'�B(�B(�B(�B(�B(�B)�B)�B)�B)�B+B+B+B+B,B,B,B-B.B.B.B.B.B/B/B/B/B/B0!B1'B1'B1'B2-B2-B33B33B33B33B49B49B5?B5?B5?B6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB7LB7LB8RB8RB9XB9XB9XB9XB9XB:^B:^B:^B:^B:^B;dB;dB;dB<jB<jB=qB=qB=qB=qB=qB>wB>wB>wB>wB>wB>wB?}B?}B@�B@�BA�BA�BA�BA�BB�BB�BB�BB�BC�BC�BC�BC�BD�BD�BE�BE�BE�BE�BE�BE�BF�BF�BF�BF�BG�BG�BG�BG�BG�BG�BG�BH�BH�BH�BH�BI�BI�BI�BI�BI�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BK�BK�BL�BL�BL�BL�BM�BM�BM�BN�BN�BN�BN�BO�BO�BO�BO�BO�BP�BP�BP�BQ�BQ�BQ�BQ�BQ�BQ�BR�BR�BR�BS�BS�BS�BS�BT�BT�BT�BT�BT�BT�BT�BVBVBVBW
BW
BW
BW
BW
BW
BXBXBXBXBXBXBXBYBYBYBYBYBYBYBZBZBZBZB[#B[#444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.12 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210718090048                              AO  ARCAADJP                                                                    20210718090048    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210718090048  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210718090048  QCF$                G�O�G�O�G�O�8000            