CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       ~2019-01-03T18:37:31Z creation;2019-01-03T18:37:34Z conversion to V3.1;2019-12-18T07:17:52Z update;2022-11-21T05:29:36Z update;     
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
_FillValue                 �  ]H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �P   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΐ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190103183731  20221123111508  4902148 J-ARGO                                                          JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I1_0397_160                     2C  Dd�NAVIS_A                         0397                            ARGO 011514                     863 @؝!��� 1   @؝"[��@;��/���d��+j�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� D@��DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D� D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B���B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@��DAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTq�DT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��\D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�8�D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��\D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D�)D�)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�jA��DA��HA��/A��A���A��\A�x�A�l�A�K�A�1'A�$�A��A�oA�JA�1A���A�ĜA���A��\A�v�A�;dA�{A�bA�bA�1A���A��A��TA���A�A��hA�x�A�t�A�l�A�XA�?}A�5?A�/A��A���A��9A�A� �A�;dA���A���A���A��`A���A��A���A�\)A���A��A��;A��A��A��;A���A��mA���A�G�A�ZA��FA�S�A��A�O�A�%A��A�VA��mA��A�1A���A�t�A���A�\)A�%A��^A�-AS�A~{A{�
Az��Ay�mAx�Aw�FAw/Av�Au�As�Aq`BApAnI�Ak�Ah�9Agt�Ag
=Af1'AcƨAb��Ab�Ab�HAb�Ab(�A`�DA`�A_�A_hsA]�A\=qAZ�AY7LAX�/AW�
AU�AT��ATM�AT1ASx�AS/AS�AR��AQ�#AQK�APJAN�!AM�AK��AJ��AI�TAI�hAIhsAIC�AI�AH��AHv�AG��AFr�AC��AA��A@��A@Q�A?"�A>�A>��A=�mA<�A;l�A:��A9�wA8��A7�A7;dA6��A5��A4�A3�A2��A2�A2��A2z�A25?A1�A0�9A01'A/�-A/x�A/O�A/
=A-�TA,�A+��A+&�A*ZA)��A)��A)G�A(�A(9XA'+A&�uA& �A%hsA$�/A$^5A#�
A#�wA#A" �A!&�A7LAbNA�/A�FA%A|�A1'A�A�^A��Ar�A�A��AO�AȴA^5A5?AA��A|�A/A��A�+AbA�A�^AdZA��A-A�HA�A�7A
��A
�A�HA�`Ax�A��A�^AS�AVA��A�DAZA1'AbA�mA�^A��Ax�A`BA;dA
=A ĜA �\A jA 9X@��@��@���@��^@���@�E�@��y@��y@�^5@��@�Ĝ@�?}@�\)@��@�D@�@�@�&�@��/@��@�X@�Q�@�+@ڟ�@�-@���@ٺ^@�?}@�Q�@�33@���@֧�@�$�@�V@�C�@���@мj@�t�@�@�r�@ɲ-@�G�@��@���@��`@ȼj@ȃ@�j@�9X@��;@Ǯ@Ǯ@�33@�^5@���@�S�@�v�@�5?@��^@���@�1@�t�@�\)@�+@�@���@�5?@���@���@��@�G�@�v�@�-@��@�hs@���@�Ĝ@��F@���@�@�&�@�V@��9@�Q�@��@�$�@��@�7L@�;d@�?}@���@���@���@�I�@��;@��w@��@���@��P@�|�@�dZ@��@�v�@���@���@���@��h@�O�@���@��D@�t�@��!@��@��-@��`@�j@���@�ȴ@�~�@�{@���@�hs@���@�bN@�(�@�ƨ@�o@��#@�V@�Ĝ@��j@�Z@��@���@��
@���@�
=@��H@��R@���@���@��+@�n�@�M�@��T@�x�@��/@���@�l�@�ȴ@���@��h@��@�j@�|�@�o@�~�@�$�@�@�O�@�V@��/@��@�j@�(�@��
@��P@�dZ@��@��@�`B@�G�@��@��/@��9@��@�(�@���@�33@��@�$�@��T@��7@�G�@�/@��@�%@���@��`@���@��@�z�@�A�@�@�P@l�@;d@~V@}?}@|�@|I�@{�@{"�@z�@z�!@y�@yx�@x��@x��@x��@x��@xbN@w�@w|�@w�@v��@vE�@v$�@v$�@v{@u/@t�D@tI�@s��@s��@sdZ@s33@s@r��@rn�@r=q@r�@rJ@q��@q�#@q��@q��@qx�@qX@q&�@q%@p��@p��@pr�@pA�@pb@o�;@ol�@o�@n��@n��@nV@n@m�-@m`B@m/@m�@l�/@l�@lI�@l1@k�
@kƨ@k��@kdZ@k33@ko@j�H@j��@j�\@j�\@jn�@i��@i�@i%@h�`@h��@hr�@hQ�@g�;@g�P@g\)@f�R@fff@f{@e@e`B@d��@d�j@d��@dZ@d1@c�
@c�F@c��@cS�@cC�@cC�@c"�@c@b�H@b�\@bn�@b=q@a�^@a&�@`��@`�@`r�@`A�@_�@^�y@^ff@]��@]/@\��@\z�@\9X@[��@[ƨ@[�F@[��@["�@Z�@Z�\@Z-@Y��@Y��@Y�7@X��@X �@Vȴ@V$�@UV@T�@S�
@S��@St�@SC�@R�@R��@R��@R��@R�!@R�!@R�\@R~�@RM�@RJ@Q�7@QG�@Q7L@Q7L@Q%@P�@P�@PbN@Pb@O|�@N�y@Nv�@Nv�@Nff@NV@N5?@M�T@M�h@M/@L�@L(�@K�
@J��@J=q@I��@I�#@I��@I&�@HA�@G�w@G�@F�y@F�R@FE�@E��@D�@Dz�@Dj@D1@C@B��@B�\@BM�@B=q@A�#@A�7@A�7@A�7@Ax�@@Ĝ@@Q�@?�@?�@?�P@?l�@?;d@?
=@>�@>�R@>��@>ff@>V@>@=�-@=O�@=�@<�@<�@<��@<�D@<j@<1@;��@;C�@;@:�\@:=q@9��@9�#@9�^@9�7@9hs@9X@8��@8��@8bN@8r�@8bN@8bN@8bN@8Q�@8A�@7�;@7+@6{@5�-@5��@5��@5�@5/@4z�@4�@3�F@3C�@3"�@3@2�H@2��@2��@2=q@1�@1��@1x�@1G�@17L@1%@0�9@0�@0Q�@0A�@0 �@/��@/l�@/K�@/K�@/+@/�@.��@.��@.��@.��@.ȴ@.�R@.��@.��@.��@.��@.��@.��@.V@.E�@.5?@.{@.{@-�-@-�@-O�@-?}@-/@,��@,�@,I�@,�@+�
@+��@+S�@+o@*��@*J@)��@)x�@(��@(��@(��@(Ĝ@(�u@(  @'�@';d@&�@&�R@&V@%��@%?}@%V@$��@$��@$�D@$z�@$(�@#C�@"�\@!x�@ Ĝ@ Q�@  �@��@\)@;d@+@�@�@
=@
=@�@�R@��@��@ff@V@V@$�@@�T@��@@�-@��@`B@?}@/@/@/@�@V@�/@�@z�@Z@Z@Z@9X@(�@1@ƨ@�@o@�@�#@��@�^@��@x�@X@G�@G�@7L@%@Ĝ@�@Q�@ �@  @�;@�@�@v�@ff@�@@��@�h@�@`B@`B@`B@`B@`B@/@V@�/@��@�@�D@j@Z@�
@C�@"�@�@�H@��@n�@^5@M�@=q@�@�@��@�#@�7@�`@�@r�@1'@  @�@�;@��@�@|�@|�@�P@|�@l�@l�@l�@l�@l�@K�@
=@�@��@5?@�T@�h@O�@�j@��@j@I�@�@�
@��@t�@33@
�@
��@
�!@
�@	X@	&�@	%@�`@�`@Ĝ@�u@�@bN@1'@b@b@b@  @  @  @�@�@�;@��@|�@\)@K�@;d@�@�@�@��@��@5?@�@�-@O�@�D@9X@�@1@�m@�
@ƨ@��@��@��@��@�@�@�@dZ@dZ@dZ@dZ@dZ@dZ@dZ@dZ@C�@S�@S�@S�@S�@S�@C�@C�@o@�@��@��@ ��@ �@ �@ �@ r�@ bN@ 1'?��;?��?�v�?�p�?�/?���?�I�?�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�jA��DA��HA��/A��A���A��\A�x�A�l�A�K�A�1'A�$�A��A�oA�JA�1A���A�ĜA���A��\A�v�A�;dA�{A�bA�bA�1A���A��A��TA���A�A��hA�x�A�t�A�l�A�XA�?}A�5?A�/A��A���A��9A�A� �A�;dA���A���A���A��`A���A��A���A�\)A���A��A��;A��A��A��;A���A��mA���A�G�A�ZA��FA�S�A��A�O�A�%A��A�VA��mA��A�1A���A�t�A���A�\)A�%A��^A�-AS�A~{A{�
Az��Ay�mAx�Aw�FAw/Av�Au�As�Aq`BApAnI�Ak�Ah�9Agt�Ag
=Af1'AcƨAb��Ab�Ab�HAb�Ab(�A`�DA`�A_�A_hsA]�A\=qAZ�AY7LAX�/AW�
AU�AT��ATM�AT1ASx�AS/AS�AR��AQ�#AQK�APJAN�!AM�AK��AJ��AI�TAI�hAIhsAIC�AI�AH��AHv�AG��AFr�AC��AA��A@��A@Q�A?"�A>�A>��A=�mA<�A;l�A:��A9�wA8��A7�A7;dA6��A5��A4�A3�A2��A2�A2��A2z�A25?A1�A0�9A01'A/�-A/x�A/O�A/
=A-�TA,�A+��A+&�A*ZA)��A)��A)G�A(�A(9XA'+A&�uA& �A%hsA$�/A$^5A#�
A#�wA#A" �A!&�A7LAbNA�/A�FA%A|�A1'A�A�^A��Ar�A�A��AO�AȴA^5A5?AA��A|�A/A��A�+AbA�A�^AdZA��A-A�HA�A�7A
��A
�A�HA�`Ax�A��A�^AS�AVA��A�DAZA1'AbA�mA�^A��Ax�A`BA;dA
=A ĜA �\A jA 9X@��@��@���@��^@���@�E�@��y@��y@�^5@��@�Ĝ@�?}@�\)@��@�D@�@�@�&�@��/@��@�X@�Q�@�+@ڟ�@�-@���@ٺ^@�?}@�Q�@�33@���@֧�@�$�@�V@�C�@���@мj@�t�@�@�r�@ɲ-@�G�@��@���@��`@ȼj@ȃ@�j@�9X@��;@Ǯ@Ǯ@�33@�^5@���@�S�@�v�@�5?@��^@���@�1@�t�@�\)@�+@�@���@�5?@���@���@��@�G�@�v�@�-@��@�hs@���@�Ĝ@��F@���@�@�&�@�V@��9@�Q�@��@�$�@��@�7L@�;d@�?}@���@���@���@�I�@��;@��w@��@���@��P@�|�@�dZ@��@�v�@���@���@���@��h@�O�@���@��D@�t�@��!@��@��-@��`@�j@���@�ȴ@�~�@�{@���@�hs@���@�bN@�(�@�ƨ@�o@��#@�V@�Ĝ@��j@�Z@��@���@��
@���@�
=@��H@��R@���@���@��+@�n�@�M�@��T@�x�@��/@���@�l�@�ȴ@���@��h@��@�j@�|�@�o@�~�@�$�@�@�O�@�V@��/@��@�j@�(�@��
@��P@�dZ@��@��@�`B@�G�@��@��/@��9@��@�(�@���@�33@��@�$�@��T@��7@�G�@�/@��@�%@���@��`@���@��@�z�@�A�@�@�P@l�@;d@~V@}?}@|�@|I�@{�@{"�@z�@z�!@y�@yx�@x��@x��@x��@x��@xbN@w�@w|�@w�@v��@vE�@v$�@v$�@v{@u/@t�D@tI�@s��@s��@sdZ@s33@s@r��@rn�@r=q@r�@rJ@q��@q�#@q��@q��@qx�@qX@q&�@q%@p��@p��@pr�@pA�@pb@o�;@ol�@o�@n��@n��@nV@n@m�-@m`B@m/@m�@l�/@l�@lI�@l1@k�
@kƨ@k��@kdZ@k33@ko@j�H@j��@j�\@j�\@jn�@i��@i�@i%@h�`@h��@hr�@hQ�@g�;@g�P@g\)@f�R@fff@f{@e@e`B@d��@d�j@d��@dZ@d1@c�
@c�F@c��@cS�@cC�@cC�@c"�@c@b�H@b�\@bn�@b=q@a�^@a&�@`��@`�@`r�@`A�@_�@^�y@^ff@]��@]/@\��@\z�@\9X@[��@[ƨ@[�F@[��@["�@Z�@Z�\@Z-@Y��@Y��@Y�7@X��@X �@Vȴ@V$�@UV@T�@S�
@S��@St�@SC�@R�@R��@R��@R��@R�!@R�!@R�\@R~�@RM�@RJ@Q�7@QG�@Q7L@Q7L@Q%@P�@P�@PbN@Pb@O|�@N�y@Nv�@Nv�@Nff@NV@N5?@M�T@M�h@M/@L�@L(�@K�
@J��@J=q@I��@I�#@I��@I&�@HA�@G�w@G�@F�y@F�R@FE�@E��@D�@Dz�@Dj@D1@C@B��@B�\@BM�@B=q@A�#@A�7@A�7@A�7@Ax�@@Ĝ@@Q�@?�@?�@?�P@?l�@?;d@?
=@>�@>�R@>��@>ff@>V@>@=�-@=O�@=�@<�@<�@<��@<�D@<j@<1@;��@;C�@;@:�\@:=q@9��@9�#@9�^@9�7@9hs@9X@8��@8��@8bN@8r�@8bN@8bN@8bN@8Q�@8A�@7�;@7+@6{@5�-@5��@5��@5�@5/@4z�@4�@3�F@3C�@3"�@3@2�H@2��@2��@2=q@1�@1��@1x�@1G�@17L@1%@0�9@0�@0Q�@0A�@0 �@/��@/l�@/K�@/K�@/+@/�@.��@.��@.��@.��@.ȴ@.�R@.��@.��@.��@.��@.��@.��@.V@.E�@.5?@.{@.{@-�-@-�@-O�@-?}@-/@,��@,�@,I�@,�@+�
@+��@+S�@+o@*��@*J@)��@)x�@(��@(��@(��@(Ĝ@(�u@(  @'�@';d@&�@&�R@&V@%��@%?}@%V@$��@$��@$�D@$z�@$(�@#C�@"�\@!x�@ Ĝ@ Q�@  �@��@\)@;d@+@�@�@
=@
=@�@�R@��@��@ff@V@V@$�@@�T@��@@�-@��@`B@?}@/@/@/@�@V@�/@�@z�@Z@Z@Z@9X@(�@1@ƨ@�@o@�@�#@��@�^@��@x�@X@G�@G�@7L@%@Ĝ@�@Q�@ �@  @�;@�@�@v�@ff@�@@��@�h@�@`B@`B@`B@`B@`B@/@V@�/@��@�@�D@j@Z@�
@C�@"�@�@�H@��@n�@^5@M�@=q@�@�@��@�#@�7@�`@�@r�@1'@  @�@�;@��@�@|�@|�@�P@|�@l�@l�@l�@l�@l�@K�@
=@�@��@5?@�T@�h@O�@�j@��@j@I�@�@�
@��@t�@33@
�@
��@
�!@
�@	X@	&�@	%@�`@�`@Ĝ@�u@�@bN@1'@b@b@b@  @  @  @�@�@�;@��@|�@\)@K�@;d@�@�@�@��@��@5?@�@�-@O�@�D@9X@�@1@�m@�
@ƨ@��@��@��@��@�@�@�@dZ@dZ@dZ@dZ@dZ@dZ@dZ@dZ@C�@S�@S�@S�@S�@S�@C�@C�@o@�@��@��@ ��@ �@ �@ �@ r�@ bN@ 1'?��;?��?�v�?�p�?�/?���?�I�?�I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��BɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�#B�/B�;B�HB�NB�NB�NB�NB�TB�TB�ZB�fB�B�B�B��B��B��B��B��B�B�B�fB��B��BhsB]/BN�B5?B$�B�BhB
=B��B�B�`B�
B��B�!B��B�B^5BK�B7LB(�B�B�BVB+BB
��B
��B
�B
�/B
��B
�RB
�3B
��B
��B
��B
��B
�JB
�B
x�B
jB
aHB
[#B
Q�B
J�B
E�B
>wB
6FB
,B
�B
{B
1B	��B	�sB	�;B	�)B	�B	��B	ƨB	ŢB	ĜB	ÖB	��B	�RB	�FB	�9B	�!B	��B	��B	��B	�hB	�VB	�7B	|�B	w�B	u�B	s�B	p�B	o�B	n�B	l�B	gmB	cTB	^5B	VB	N�B	F�B	A�B	>wB	<jB	;dB	:^B	9XB	8RB	6FB	2-B	'�B	�B	bB	\B	�B	�B	�B	�B	hB		7B	B	  B��B��B�B�B�yB�TB�B��B��B��B��B��B��BɺBƨBĜBÖBB��B�}B�RB�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�\B�DB�+B�B}�By�Bv�Bs�Bp�Bl�BhsBe`BcTBbNB`BB_;B^5B]/B\)B[#B[#BZBYBXBW
BVBVBT�BS�BR�BP�BN�BK�BI�BH�BE�BC�B?}B;dB8RB6FB5?B49B49B33B2-B2-B2-B1'B1'B1'B0!B0!B0!B0!B/B/B.B.B-B-B,B+B(�B%�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B{B{BuBuBuBuBoBoBoBoBoBhBoBoBoBoBoBhBhB{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B)�B+B+B,B.B-B/B1'B33B5?B5?B6FB7LB9XB>wB>wB?}BE�BL�BM�BM�BN�BO�BP�BP�BP�BQ�BQ�BQ�BQ�BR�BT�BW
BW
BXBXBYBZB[#B`BBcTBe`BgmBjBk�Bo�Bs�Bt�Bw�Bx�Bz�B}�B� B�B�B�+B�PB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�FB�qB�}BBɺB��B�B�B�/B�;B�NB�ZB�`B�mB�sB�B�B�B�B�B��B	B	B	B	B	%B	+B	
=B	\B	uB	�B	�B	�B	 �B	#�B	$�B	%�B	%�B	%�B	&�B	'�B	)�B	+B	-B	0!B	1'B	2-B	2-B	6FB	<jB	?}B	@�B	D�B	E�B	F�B	G�B	K�B	M�B	P�B	Q�B	Q�B	Q�B	R�B	VB	XB	ZB	\)B	^5B	^5B	^5B	^5B	bNB	e`B	ffB	hsB	iyB	k�B	l�B	m�B	n�B	o�B	p�B	q�B	q�B	r�B	r�B	r�B	s�B	t�B	t�B	u�B	v�B	w�B	x�B	y�B	z�B	z�B	{�B	~�B	� B	� B	�B	�B	�B	�%B	�1B	�1B	�7B	�=B	�DB	�JB	�PB	�VB	�VB	�\B	�bB	�hB	�hB	�oB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�'B	�'B	�'B	�3B	�3B	�9B	�FB	�RB	�XB	�^B	�dB	�jB	�wB	��B	B	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�/B	�BB	�NB	�TB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
1B
	7B
	7B
	7B
JB
JB
PB
PB
VB
VB
\B
\B
\B
\B
hB
hB
oB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
#�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
)�B
+B
,B
,B
,B
,B
-B
,B
.B
.B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
B�B
B�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
H�B
I�B
J�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
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
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
VB
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
XB
XB
XB
XB
XB
XB
ZB
ZB
ZB
[#B
[#B
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
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
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
dZB
e`B
e`B
e`B
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
jB
jB
jB
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
p�B
p�B
q�B
q�B
q�B
r�B
s�B
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
u�B
u�B
u�B
u�B
u�B
w�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
{�B
{�B
|�B
|�B
|�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BΥB��B��B��B�0B�6B�B��B�B�B�B��B�B��B�B�(B�\B�gB�eBیBݲBߤB�HB�hB�hB�B�hB�B�B�B��B��B�B��B��B��B��B�B�%B�B��B�0B�B��BjKB`\BSuB8�B'8B�B�BjB OB�]B��B�#B�mB��B�4B��Ba�BN�B9XB*B�B�B\B�BB
��B
�FB
��B
��B
�<B
��B
��B
�B
��B
��B
��B
��B
��B
{0B
k�B
b�B
\�B
SB
K�B
GB
@ B
8lB
.�B
�B
$B
�B	��B	��B	�BB	��B	�_B	˒B	��B	��B	�B	��B	�B	��B	��B	�tB	�B	�KB	��B	��B	�TB	�B	��B	~B	xRB	v`B	tnB	qB	pB	o5B	m�B	h�B	e,B	`B	XB	P�B	G�B	B[B	>�B	<�B	;�B	:�B	9�B	9	B	7�B	4�B	+6B	�B	�B	HB	�B	7B	B	�B	@B	
�B	9B	UB�VB��B��B��B�B�B�B�.B�.B�HBуB�vB��B��B�_B�SB��B��B�AB�B��B��B��B��B��B�B�vB�pB��B�B��B�xB��B�yB�_B�SB�B��B��B��B�lB��B� B{BxBu�Br|BnIBjBf�Bd@BcB`�B_�B^�B]�B\xB[�B[�BZ�BY�BX�BW�BV�BVmBU�BT�BS�BRBP}BMBJ�BJ	BGBE�BA�B=VB9�B7�B5�B4�B4�B3�B2|B2�B2|B1�B1�B1vB0�B0�B0�B0�B/�B/�B.cB.}B-�B-�B,�B,WB+�B(sB$@B!�BVB�B�B�B�B�BeB�ByB�B
BmBB2B2B�B�B�B�B�B&BB�B�B�BTB�BuBuB[B�B�B�B�B�B�B�B�B�B�B�B�B�B�BBSB�B�BQB�B=B=BqB)B�B�B�B/B~BBOBjB B$ZB*eB+QB+�B,�B.}B-�B/�B1�B3�B5�B5�B6�B8B:DB>�B?cB@�BF�BM6BNBN<BOBBPHBQBQ BQBRBR:BR:BRoBS�BU�BWYBW?BXEBXyBYBZ�B[�B`�Bc�Be�Bh
BkBl=Bp;BtBu%Bx8ByXB{B~]B�iB��B��B�B��B��B��B��B��B��B��B�B�5B�B�&B�B��B��B�B�B�RB�RB��B��B��B��B�B��B� B�GB�rB�[B�mB�kBݘBߤB�B�B�B�B��B��B��B��B�'B�nB�dB	 B	UB	[B	gB	tB	�B	
�B	�B	�B	9B	�B	B	!B	#�B	%B	&B	%�B	%�B	'B	(>B	*KB	+6B	-CB	0UB	1AB	2|B	2�B	6�B	<�B	?�B	@�B	D�B	E�B	F�B	HB	K�B	NB	QB	RB	R B	R:B	S@B	V9B	X_B	ZkB	\xB	^OB	^OB	^jB	^�B	b�B	e�B	f�B	h�B	i�B	k�B	l�B	m�B	n�B	o�B	p�B	q�B	q�B	r�B	r�B	r�B	s�B	t�B	t�B	u�B	v�B	xB	x�B	zB	z�B	{B	|B	B	�B	�4B	�[B	�aB	�9B	�?B	�fB	�fB	�lB	�rB	�xB	�dB	�jB	��B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�8B	�>B	�B	�0B	�6B	�=B	�CB	�/B	�/B	�OB	�;B	�;B	�;B	�[B	�[B	�[B	�hB	�hB	�nB	�zB	�lB	��B	�xB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�hB	�YB	ؓB	ܒB	ݲB	�B	�hB	�B	�tB	�B	�B	�B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�LB	�B	�B	�B	�B	�<B	�BB
UB
AB
-B
-B
aB
gB
mB
fB
	RB
	�B
	�B
~B
~B
jB
�B
�B
�B
\B
vB
vB
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"B
#:B
$&B
%�B
'B
&�B
'B
'B
($B
)DB
*KB
+6B
,=B
,=B
,=B
,=B
-)B
,WB
.IB
./B
/5B
/OB
/OB
/OB
0UB
0UB
1[B
1[B
1[B
1[B
3MB
3MB
33B
3hB
3MB
4TB
4TB
49B
4TB
4nB
5?B
5ZB
5?B
5ZB
5ZB
5?B
5tB
5ZB
6`B
6`B
6zB
6zB
6`B
7�B
7fB
7fB
7�B
8�B
8�B
8�B
9rB
9rB
:�B
:xB
;�B
;�B
;�B
=�B
=�B
>�B
?�B
?�B
?�B
?�B
?�B
@�B
A�B
A�B
B�B
B�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
HB
I7B
J	B
J�B
K�B
LB
L�B
M�B
M�B
M�B
M�B
M�B
M�B
NB
N�B
N�B
N�B
OB
N�B
O�B
PB
O�B
PB
O�B
O�B
Q B
Q B
QB
Q B
Q B
Q B
RB
Q�B
R B
RB
RB
RB
SB
R�B
SB
S&B
S&B
S&B
S&B
S&B
TFB
TaB
UB
VB
VB
V9B
V9B
VB
W
B
W$B
W?B
W?B
W?B
W?B
X+B
X+B
X+B
X+B
XEB
XyB
Z7B
ZQB
ZkB
[WB
[WB
[=B
[=B
\CB
\CB
\CB
\CB
\)B
\CB
\CB
\CB
]/B
]dB
]dB
]IB
]dB
]dB
^�B
_pB
_pB
_pB
_VB
`vB
`\B
`BB
`\B
`vB
`\B
`vB
`vB
a�B
a|B
bhB
bhB
c�B
cnB
cTB
cnB
cnB
c�B
dtB
dZB
dtB
dZB
dtB
dZB
dZB
dtB
dtB
d�B
d�B
e�B
e�B
e�B
f�B
f�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
j�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
p�B
o�B
p�B
p�B
q�B
q�B
q�B
r�B
s�B
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
u�B
u�B
u�B
vB
vFB
x8B
y	B
y�B
y�B
y�B
zB
y�B
zB
zB
z�B
|6B
|B
}B
}B
|�B
}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.12(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901140034182019011400341820190114003418202211182137342022111821373420221118213734201901150016292019011500162920190115001629  JA  ARFMdecpA19c                                                                20190104033634  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190103183731  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190103183733  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190103183733  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190103183734  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190103183734  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190103183734  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190103183734  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190103183734  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190103183734                      G�O�G�O�G�O�                JA  ARUP                                                                        20190103185559                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190103153241  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20190113153418  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190113153418  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190114151629  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200114231517                      G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20221118123734  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20221123111508                      G�O�G�O�G�O�                