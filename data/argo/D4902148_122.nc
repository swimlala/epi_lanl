CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2017-12-19T15:35:33Z creation;2017-12-19T15:35:36Z conversion to V3.1;2019-12-18T07:26:07Z update;2022-11-21T05:31:31Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7\   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7`   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7p   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  8   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :    PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]\   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aH   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ސ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20171219153533  20221123111507  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               zA   JA  I1_0397_122                     2C  Dd�NAVIS_A                         0397                            ARGO 011514                     863 @�> �m�1   @�> ��� @;J^5?|��d����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  D   D �fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@%�@xQ�@�(�@�(�A{A>{A^{A|z�A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B���B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��D ~�D �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111S�A�{A�&�A�+A�+A�-A�7LA�9XA�+A�/A�33A�33A�7LA�9XA�;dA�?}A�9XA�7LA�1'A�7LA�5?A�7LA�?}A�C�A�I�A�M�A�M�A�O�A�O�A�M�A�Q�A�Q�A�Q�A�S�A�VA�VA�XA�VA�XA�XA�XA�XA�XA�ZA�VA�ZA�ZA�VA�O�A�Q�A�S�A�S�A�VA�Q�A� �A���A��;A��A�r�A�A�A��9A�t�A��uA��yA���A��`A���A��A��FA�7LA��wA��7A�`BA�%A�+A��yA�G�A�/A��A�|�A�A��`A�ƨA��^A���A�|�A�+A��A�M�A�C�A��-A�C�A��A~�/A}�A{\)Ayl�Ax��Aw��AvQ�Au�;Au�-Au�7At�Ao�^Ak�PAh�Af�Af �Ae��Ae�Ae��Ae�^Ae��Ad�Ad1Aa�A`�RA_�;A_��A_\)A^ffA]�-A\��AZ�`AX5?AW?}AV~�AT��AS�AS��AS�ASt�AS/AR�AR�\AR~�AR1'AR�AQ�AQS�AP�+AOO�AN(�AMG�ALJAJĜAI�wAI+AH�AH�+AHE�AGK�AE��AEG�AD�AD�!AC��AB��AA��AA%A@�A@��A@v�A@I�A@1'A?��A?/A>�A=p�A<��A9�-A8�DA8  A57LA2�DA1��A1&�A.�A.=qA-��A,bNA*r�A(��A(1A'&�A&ZA%�wA$��A#�
A"VA!�A!dZA!�A!oA ��A �+A $�A�FAhsA9XA��A�mA��AC�A5?A�A&�AbNA��A�A��A�jA�hA33A�A��Az�An�A9XA$�A��A�A�A�
A��Al�A�A�\A
�`A�AffA�A��A�A1A|�A�A��A�A��A��A z�@���@�ff@��T@�?}@�1'@��@��@���@���@���@��w@�t�@�
=@�ȴ@�v�@��@�/@��@�j@�  @��@�5?@�ƨ@��@��@�t�@�+@�R@柾@�V@�G�@��`@�9@�@�"�@�^5@�Z@��@��@��`@ۮ@��@�S�@�=q@�%@�S�@�p�@��@���@��@�X@̼j@��@�+@ʇ+@�@Ɂ@ȣ�@�o@�x�@�Ĝ@�Z@��;@Å@���@��T@��/@���@��@�v�@�$�@���@���@���@�1'@�C�@�V@���@��D@�(�@�  @�S�@���@�=q@�%@�1'@���@�n�@���@�X@�Z@���@�r�@��
@���@�+@�M�@��h@�&�@��@��9@�Q�@��w@�l�@�t�@�dZ@��y@�J@�C�@��@�Ĝ@���@�bN@��
@�t�@�33@��R@��@�hs@��j@��P@�-@��7@�O�@�&�@�Ĝ@��j@��@��u@�bN@�Z@�I�@�(�@��@�b@�1@�  @�  @���@���@��F@��@��P@�t�@�dZ@�C�@��@�{@���@�`B@���@� �@��
@��@�33@��H@�ff@�@�hs@���@�%@���@���@�ƨ@�+@�o@��@��!@�$�@�$�@��@��^@�hs@��@�%@���@���@���@��@��u@�r�@�j@�j@�Z@�A�@�1@��m@��w@�;d@�{@���@�@���@���@��h@���@��D@�A�@��@��w@��P@�t�@�S�@�+@�@�ȴ@�^5@���@��@��!@�&�@��j@�j@�;@�P@�w@�1@�1@�@��@|�@\)@~�y@~��@~�@~ff@}@}�h@}�@}?}@|�/@|�@{�m@{ƨ@{�F@{�F@{�F@{�@z�@z�@y�7@y��@x�9@w�w@w|�@v�R@vv�@v5?@v{@u@u�@u`B@u�@t�@t�j@tZ@s@r��@r��@r��@r��@r^5@r�@r�@q��@qhs@p��@o��@n{@l��@lI�@l(�@k��@j��@jn�@j�@ihs@i&�@i�@h��@hĜ@h�9@h��@h�u@hQ�@h  @g�;@g�w@g�P@gl�@g|�@gK�@f��@f��@f��@f��@f�+@fE�@fE�@fE�@f$�@e�@e�T@eV@d�D@dZ@c�
@cdZ@b�H@b��@bM�@a��@ahs@a7L@a&�@a%@a�@`��@`��@`�@`  @_\)@^��@^v�@\��@[�m@Z�!@ZJ@Y�@Y�#@Y��@Y��@Y��@Y�7@Y�7@Y�7@Y�7@Yhs@YX@Y7L@Y&�@XĜ@W�@Wl�@W\)@W;d@W
=@Vȴ@V�R@V�+@U��@Up�@UV@T�@T9X@S��@S33@R^5@Q��@QG�@Q%@P�`@P�9@P�u@P1'@O+@N�+@M��@M/@L��@L�@L�/@L��@L��@Lj@K��@K�@K@I��@H��@H1'@Hb@G�;@G�@G�P@G\)@G+@F��@FE�@E�@E�h@E`B@D�@D�j@Dz�@Dz�@DZ@B��@A�7@A7L@A&�@@��@@ �@?�;@?�P@?K�@?�@?
=@>�y@>��@>ff@>{@=�@=@=p�@=�@<�@<z�@<j@<1@;ƨ@;ƨ@;ƨ@;��@;��@;��@;t�@:�H@:=q@:J@9�^@9x�@9hs@9X@9G�@9�@8��@8r�@8A�@8b@7�@7�w@7;d@7�@7
=@7
=@7
=@7
=@7
=@6��@6��@6�y@6�y@6�y@6ȴ@6�R@6�R@6�+@6v�@6ff@6ff@6{@5��@5`B@5?}@5�@4�@4��@4Z@3t�@3dZ@333@2��@2�!@1X@0�9@0�u@0b@.��@.��@.V@.E�@.$�@.{@-�@-@-�h@-p�@-p�@-?}@-�@,��@,�j@,Z@+��@+"�@+o@*�@*�!@*^5@)�#@)x�@)�@(�@'�w@';d@&�R@%�@$��@#��@#�F@#��@#dZ@#@"�!@!��@!&�@ ��@ b@�@��@�@�@�P@�P@l�@;d@+@
=@��@�@�R@�@`B@/@/@�@�/@�j@j@�
@t�@S�@C�@"�@�@��@��@~�@^5@-@��@��@��@X@G�@7L@7L@�@��@�`@�u@A�@�@��@|�@l�@;d@+@+@��@ȴ@ȴ@��@ff@E�@{@�T@�T@��@��@��@��@�T@��@�-@�h@`B@?}@��@�/@�/@�j@��@I�@I�@(�@(�@1@�m@33@@�@�H@�H@�H@�H@�H@��@��@��@��@��@��@��@n�@=q@�@�#@��@X@&�@%@��@Ĝ@�9@�9@��@r�@b@\)@�y@ȴ@v�@5?@5?@5?@@�@��@�@/@/@�@�@�@V@V@��@�@�/@�/@�/@�/@�@�/@�/@�@�/@�/@�/@��@��@z�@(�@ƨ@�F@��@t�@S�@C�@o@
�!@
^5@
=q@
J@	�#@	hs@	&�@�u@b@��@|�@�R@��@�+@v�@ff@E�@{@@p�@/@/@/@/@/@��@�/@�@�D@Z@(�@��@��@��@dZ@C�@33@"�@o@o@@@@@@�@�@�@�@��@�\@~�@n�@~�@~�@~�@�\@n�@^5@M�@M�@M�@M�@-@=q@=q@�@�#@�^@�^@�^@��@��@��@�7@X@X@7L@�@�@%@%@ ��@ ��@ ��@ ��@ �@ �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111S�A�{A�&�A�+A�+A�-A�7LA�9XA�+A�/A�33A�33A�7LA�9XA�;dA�?}A�9XA�7LA�1'A�7LA�5?A�7LA�?}A�C�A�I�A�M�A�M�A�O�A�O�A�M�A�Q�A�Q�A�Q�A�S�A�VA�VA�XA�VA�XA�XA�XA�XA�XA�ZA�VA�ZA�ZA�VA�O�A�Q�A�S�A�S�A�VA�Q�A� �A���A��;A��A�r�A�A�A��9A�t�A��uA��yA���A��`A���A��A��FA�7LA��wA��7A�`BA�%A�+A��yA�G�A�/A��A�|�A�A��`A�ƨA��^A���A�|�A�+A��A�M�A�C�A��-A�C�A��A~�/A}�A{\)Ayl�Ax��Aw��AvQ�Au�;Au�-Au�7At�Ao�^Ak�PAh�Af�Af �Ae��Ae�Ae��Ae�^Ae��Ad�Ad1Aa�A`�RA_�;A_��A_\)A^ffA]�-A\��AZ�`AX5?AW?}AV~�AT��AS�AS��AS�ASt�AS/AR�AR�\AR~�AR1'AR�AQ�AQS�AP�+AOO�AN(�AMG�ALJAJĜAI�wAI+AH�AH�+AHE�AGK�AE��AEG�AD�AD�!AC��AB��AA��AA%A@�A@��A@v�A@I�A@1'A?��A?/A>�A=p�A<��A9�-A8�DA8  A57LA2�DA1��A1&�A.�A.=qA-��A,bNA*r�A(��A(1A'&�A&ZA%�wA$��A#�
A"VA!�A!dZA!�A!oA ��A �+A $�A�FAhsA9XA��A�mA��AC�A5?A�A&�AbNA��A�A��A�jA�hA33A�A��Az�An�A9XA$�A��A�A�A�
A��Al�A�A�\A
�`A�AffA�A��A�A1A|�A�A��A�A��A��A z�@���@�ff@��T@�?}@�1'@��@��@���@���@���@��w@�t�@�
=@�ȴ@�v�@��@�/@��@�j@�  @��@�5?@�ƨ@��@��@�t�@�+@�R@柾@�V@�G�@��`@�9@�@�"�@�^5@�Z@��@��@��`@ۮ@��@�S�@�=q@�%@�S�@�p�@��@���@��@�X@̼j@��@�+@ʇ+@�@Ɂ@ȣ�@�o@�x�@�Ĝ@�Z@��;@Å@���@��T@��/@���@��@�v�@�$�@���@���@���@�1'@�C�@�V@���@��D@�(�@�  @�S�@���@�=q@�%@�1'@���@�n�@���@�X@�Z@���@�r�@��
@���@�+@�M�@��h@�&�@��@��9@�Q�@��w@�l�@�t�@�dZ@��y@�J@�C�@��@�Ĝ@���@�bN@��
@�t�@�33@��R@��@�hs@��j@��P@�-@��7@�O�@�&�@�Ĝ@��j@��@��u@�bN@�Z@�I�@�(�@��@�b@�1@�  @�  @���@���@��F@��@��P@�t�@�dZ@�C�@��@�{@���@�`B@���@� �@��
@��@�33@��H@�ff@�@�hs@���@�%@���@���@�ƨ@�+@�o@��@��!@�$�@�$�@��@��^@�hs@��@�%@���@���@���@��@��u@�r�@�j@�j@�Z@�A�@�1@��m@��w@�;d@�{@���@�@���@���@��h@���@��D@�A�@��@��w@��P@�t�@�S�@�+@�@�ȴ@�^5@���@��@��!@�&�@��j@�j@�;@�P@�w@�1@�1@�@��@|�@\)@~�y@~��@~�@~ff@}@}�h@}�@}?}@|�/@|�@{�m@{ƨ@{�F@{�F@{�F@{�@z�@z�@y�7@y��@x�9@w�w@w|�@v�R@vv�@v5?@v{@u@u�@u`B@u�@t�@t�j@tZ@s@r��@r��@r��@r��@r^5@r�@r�@q��@qhs@p��@o��@n{@l��@lI�@l(�@k��@j��@jn�@j�@ihs@i&�@i�@h��@hĜ@h�9@h��@h�u@hQ�@h  @g�;@g�w@g�P@gl�@g|�@gK�@f��@f��@f��@f��@f�+@fE�@fE�@fE�@f$�@e�@e�T@eV@d�D@dZ@c�
@cdZ@b�H@b��@bM�@a��@ahs@a7L@a&�@a%@a�@`��@`��@`�@`  @_\)@^��@^v�@\��@[�m@Z�!@ZJ@Y�@Y�#@Y��@Y��@Y��@Y�7@Y�7@Y�7@Y�7@Yhs@YX@Y7L@Y&�@XĜ@W�@Wl�@W\)@W;d@W
=@Vȴ@V�R@V�+@U��@Up�@UV@T�@T9X@S��@S33@R^5@Q��@QG�@Q%@P�`@P�9@P�u@P1'@O+@N�+@M��@M/@L��@L�@L�/@L��@L��@Lj@K��@K�@K@I��@H��@H1'@Hb@G�;@G�@G�P@G\)@G+@F��@FE�@E�@E�h@E`B@D�@D�j@Dz�@Dz�@DZ@B��@A�7@A7L@A&�@@��@@ �@?�;@?�P@?K�@?�@?
=@>�y@>��@>ff@>{@=�@=@=p�@=�@<�@<z�@<j@<1@;ƨ@;ƨ@;ƨ@;��@;��@;��@;t�@:�H@:=q@:J@9�^@9x�@9hs@9X@9G�@9�@8��@8r�@8A�@8b@7�@7�w@7;d@7�@7
=@7
=@7
=@7
=@7
=@6��@6��@6�y@6�y@6�y@6ȴ@6�R@6�R@6�+@6v�@6ff@6ff@6{@5��@5`B@5?}@5�@4�@4��@4Z@3t�@3dZ@333@2��@2�!@1X@0�9@0�u@0b@.��@.��@.V@.E�@.$�@.{@-�@-@-�h@-p�@-p�@-?}@-�@,��@,�j@,Z@+��@+"�@+o@*�@*�!@*^5@)�#@)x�@)�@(�@'�w@';d@&�R@%�@$��@#��@#�F@#��@#dZ@#@"�!@!��@!&�@ ��@ b@�@��@�@�@�P@�P@l�@;d@+@
=@��@�@�R@�@`B@/@/@�@�/@�j@j@�
@t�@S�@C�@"�@�@��@��@~�@^5@-@��@��@��@X@G�@7L@7L@�@��@�`@�u@A�@�@��@|�@l�@;d@+@+@��@ȴ@ȴ@��@ff@E�@{@�T@�T@��@��@��@��@�T@��@�-@�h@`B@?}@��@�/@�/@�j@��@I�@I�@(�@(�@1@�m@33@@�@�H@�H@�H@�H@�H@��@��@��@��@��@��@��@n�@=q@�@�#@��@X@&�@%@��@Ĝ@�9@�9@��@r�@b@\)@�y@ȴ@v�@5?@5?@5?@@�@��@�@/@/@�@�@�@V@V@��@�@�/@�/@�/@�/@�@�/@�/@�@�/@�/@�/@��@��@z�@(�@ƨ@�F@��@t�@S�@C�@o@
�!@
^5@
=q@
J@	�#@	hs@	&�@�u@b@��@|�@�R@��@�+@v�@ff@E�@{@@p�@/@/@/@/@/@��@�/@�@�D@Z@(�@��@��@��@dZ@C�@33@"�@o@o@@@@@@�@�@�@�@��@�\@~�@n�@~�@~�@~�@�\@n�@^5@M�@M�@M�@M�@-@=q@=q@�@�#@�^@�^@�^@��@��@��@�7@X@X@7L@�@�@%@%@ ��@ ��@ ��@ ��@ �@ �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111$�B$�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B$�B#�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B#�B$�B$�B$�B#�B#�B#�B#�B#�B"�B"�B#�B"�B"�B"�B�B�B�B{B\B
=B��B��B��BH�B�BƨB�B�hB�Bt�Bo�Bm�BjBe`B[#BK�BC�BB�B?}B7LB%�B�B�B�BDB
��B
�B
�yB
�
B
�jB
��B
��B
��B
��B
�PB
}�B
p�B
jB
cTB
[#B
XB
VB
S�B
K�B
+B
bB	��B	�B	�B	�B	�B	�B	�B	�B	�`B	�5B	��B	��B	ƨB	ÖB	��B	�dB	�LB	�!B	��B	��B	�hB	�JB	�B	~�B	}�B	|�B	{�B	z�B	x�B	w�B	v�B	u�B	t�B	s�B	o�B	k�B	e`B	_;B	[#B	T�B	N�B	I�B	F�B	E�B	C�B	A�B	<jB	6FB	33B	1'B	0!B	,B	%�B	!�B	�B	�B	�B	�B	�B	�B	�B	�B	hB	PB		7B��B��B�B�fB�)B�B��B��B��BƨB��B�^B�?B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�DB�B� B~�B|�Bz�Bx�Bv�Bs�Bq�Bo�Bl�BiyBffBffBe`BdZBdZBcTBcTBcTBbNBbNBbNBaHB`BB^5B[#BVBO�BM�BI�BG�BF�BE�BD�BC�BC�BB�BB�B@�B?}B>wB=qB=qB<jB;dB;dB;dB:^B9XB9XB8RB8RB7LB7LB7LB6FB6FB6FB5?B5?B5?B33B0!B1'B1'B1'B1'B1'B1'B0!B0!B0!B0!B0!B/B/B.B/B.B.B.B-B.B.B.B.B/B/B1'B1'B2-B2-B2-B33B49B49B49B49B5?B6FB8RB9XB9XB9XB:^B:^B;dB<jB>wB?}B?}B?}B?}B@�B@�BA�BC�BD�BE�BG�BH�BH�BI�BJ�BK�BN�BO�BP�BR�BR�BS�BT�B[#B^5B`BB`BBaHBcTBe`BgmBgmBhsBiyBjBl�Bk�Bk�Bl�Bn�Bw�B� B�B�B�B�B�%B�+B�7B�DB�PB�\B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�?B�FB�RB�^B�dB�wB�}BÖBƨBŢBŢBǮB��B��B��B��B��B�B�B�B�#B�)B�5B�5B�;B�BB�BB�NB�TB�ZB�`B�`B�`B�mB�yB�B�B�B��B��B��B��B��B��B	  B	B	B	+B	1B		7B		7B	
=B	DB	PB	VB	hB	oB	oB	�B	(�B	)�B	+B	-B	0!B	33B	9XB	;dB	;dB	<jB	?}B	@�B	C�B	C�B	D�B	F�B	J�B	K�B	K�B	L�B	L�B	N�B	Q�B	R�B	S�B	S�B	S�B	T�B	VB	VB	XB	[#B	\)B	_;B	aHB	hsB	jB	jB	k�B	l�B	m�B	n�B	o�B	p�B	q�B	s�B	w�B	x�B	w�B	w�B	x�B	y�B	z�B	{�B	{�B	}�B	� B	�B	�%B	�7B	�=B	�=B	�JB	�VB	�\B	�bB	�oB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�9B	�?B	�9B	�?B	�FB	�LB	�XB	�dB	�qB	�wB	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�5B	�BB	�BB	�HB	�NB	�TB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
DB
VB
\B
\B
bB
hB
oB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
(�B
(�B
)�B
)�B
)�B
,B
.B
.B
.B
0!B
1'B
1'B
2-B
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
49B
49B
5?B
6FB
6FB
6FB
7LB
7LB
8RB
8RB
9XB
:^B
;dB
<jB
<jB
>wB
?}B
A�B
A�B
A�B
A�B
B�B
B�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
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
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
YB
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
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
dZB
e`B
e`B
e`B
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
iyB
iyB
iyB
jB
k�B
k�B
l�B
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
o�B
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
r�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
s�B
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
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111$�B$�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B#�B$�B#�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B$�B#�B$�B$�B$�B#�B#�B#�B#�B#�B"�B"�B#�B#B# B#TB!B�BBMB}B�B�B��B�BT�B�B�JB�+B��B�Bu�BpUBn/Bk�Bg�B]�BMBC�BCaBAoB:xB'�B1BEB�B"B
�DB
��B
��B
یB
�}B
�*B
�B
��B
�yB
��B
� B
q�B
k�B
d�B
[�B
XyB
V�B
VmB
Q�B
/�B
�B
  B	�GB	��B	��B	��B	��B	�B	�B	��B	�B	� B	��B	�+B	�3B	��B	��B	��B	��B	��B	��B	��B	�"B	��B	cB	~BB	}<B	|jB	{JB	y>B	xB	wLB	vB	u%B	t�B	p�B	mB	f�B	`�B	\�B	V�B	P.B	JrB	G+B	F?B	DMB	B�B	=�B	72B	3�B	1�B	1[B	-�B	'B	"�B	 B	!B	B	B	�B	]B	�B	�B	�B	�B	�B��B�rB�+B�_B�~B�QB�YB�B�BȚB��B�PB�`B�hB�UB�)B�eB�LB��B��B�]B�#B��B�B�B�?B�SB�{B��B��B�SB��B�B~]B{�By�BxBt�Br�BqBn/Bj�BgBf�Be�Bd�Bd�Bc�Bc�Bc�Bb�Bb�Bb�Ba�Ba-B`B]�BX�BS[BO�BKBH�BG_BFtBEmBDBC�BB�BCGBB'B@�B?cB>]B=�B=B<6B;�B;�B:�B:^B:B9	B8�B7�B7�B7�B6�B6�B6�B5�B5�B6+B5B3�B2�B1�B1�B1�B1�B1vB0�B0�B0�B0oB0�B0!B/�B/�B0B/B/B/OB.�B/OB/ B/5B/iB0�B0!B1�B1�B2�B2�B2�B4B4�B4�B4�B5%B6zB7LB8�B9�B9�B9�B;B;JB<PB=<B?.B?�B?�B?�B@ BA BA;BB[BDgBESBFtBG�BIBIRBJ#BK�BL�BO�BP�BQ�BS�BS�BT�BV�B\B^�B`�B`�Ba�Bc�Be�Bg�Bg�Bh�Bi�Bj�Bl�Bk�Bl"Bm�BpoBy$B�OB�;B�oB��B��B�tB��B��B��B�B�HB�aB�B��B��B�B��B��B�B�B��B��B�B��B��B��B��B��B�B�B�B��B�B�&B�B�B�FB��B�kB�qB��B��B��B��B��B��B��B��B��B��BƨB��B�%B�fB�BB�B�@B�FBՁB�+B�_B�kB�qB�]B�jB�jB�VB�vB��B�hB�B�B�zB�B�B�B��B��B�B�IB��B��B��B�	B�B�B	 OB	gB	mB	zB	�B		RB		lB	
rB	xB	�B	�B	:B	[B	�B	�B	)_B	*KB	+QB	-CB	0;B	33B	9rB	;B	;B	<�B	?�B	@�B	C�B	C�B	D�B	F�B	J�B	K�B	K�B	MB	M6B	OB	RB	S&B	TB	TB	TFB	UgB	VSB	V9B	XEB	[qB	\xB	_�B	a�B	h�B	j�B	j�B	k�B	l�B	m�B	n�B	o�B	p�B	rB	tB	xB	x�B	xB	w�B	y	B	y�B	{B	|B	|PB	~BB	��B	��B	��B	��B	�XB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�B	��B	�2B	�>B	�KB	�CB	�cB	�iB	�[B	�MB	�nB	�TB	�ZB	�nB	�tB	�zB	��B	��B	��B	��B	�B	�B	�7B	�B	�B	� B	� B	��B	�B	� B	�B	�B	��B	�B	�&B	�B	�,B	�,B	�MB	�_B	�7B	�7B	�7B	�WB	�CB	�]B	�xB	ބB	�vB	��B	�B	�B	�B	��B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�XB	�jB	�B
 4B
;B
;B
;B
'B
'B
AB
3B
gB
SB
YB
YB
_B
fB
KB
�B
�B
�B
pB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$B
$&B
%,B
%B
%�B
%�B
&B
&B
'8B
'8B
)B
)B
*KB
*0B
*eB
,=B
.IB
.cB
.}B
0UB
1[B
1AB
2GB
2GB
2aB
2aB
2aB
3hB
3MB
3MB
3MB
3hB
3hB
4nB
4�B
5tB
6`B
6zB
6zB
7�B
7�B
8�B
8�B
9�B
:�B
;�B
<�B
<�B
>�B
?�B
A�B
A�B
A�B
A�B
B�B
B�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
IB
J	B
J�B
K�B
K�B
K�B
K�B
K�B
MB
NB
NB
NB
N�B
N�B
N�B
N�B
N�B
O�B
PB
PB
O�B
Q B
Q B
RB
RB
RB
RB
RB
R B
RB
SB
S&B
T,B
TB
TB
T,B
UB
T�B
U2B
U2B
UB
UB
VB
VB
VB
VB
W$B
W$B
W
B
W
B
W$B
W$B
W$B
W$B
W$B
W?B
X+B
XEB
X+B
YB
X+B
Y1B
YKB
Y1B
YKB
Y1B
YKB
ZQB
ZkB
[WB
[#B
[=B
[=B
[#B
[#B
\CB
[=B
\)B
\CB
\)B
\)B
\CB
\CB
[WB
\]B
\]B
]IB
]dB
]IB
^jB
^jB
^OB
^jB
^OB
_VB
_pB
_VB
_pB
_pB
`�B
a|B
a|B
b�B
bhB
bhB
bhB
bhB
b�B
bhB
cnB
dtB
dtB
dtB
dtB
dtB
dZB
dZB
dtB
dZB
dtB
dtB
dtB
dZB
dtB
dZB
dtB
dZB
dtB
dtB
dtB
d�B
e�B
ezB
e�B
ffB
f�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
k�B
k�B
l�B
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
o�B
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
r�B
r�B
r�B
r�B
r�B
r�B
s�B
r�B
r�B
s�B
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
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<7�4<<lR<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.12(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201712310034282017123100342820171231003428202211182132592022111821325920221118213259201804031938352018040319383520180403193835  JA  ARFMdecpA19c                                                                20171220003513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171219153533  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171219153534  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171219153535  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171219153535  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171219153535  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171219153535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171219153535  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171219153536  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171219153536                      G�O�G�O�G�O�                JA  ARUP                                                                        20171219155547                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171221153436  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20171230153428  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171230153428  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180403103835  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114171536                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123259  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111507                      G�O�G�O�G�O�                