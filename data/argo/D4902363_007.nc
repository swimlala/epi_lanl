CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-06-24T09:48:26Z creation;2016-06-24T09:48:28Z conversion to V3.1;2019-12-19T08:38:08Z update;     
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160624094826  20200115101517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0576_007                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @״u�1N 1   @״v��O�@;�W���'�dl��p:�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dvy�Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�<�DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��fD��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B���B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCMǮCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvq�Dv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�\D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�\D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�8�D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D���D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�?\D���D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A��A��A��A��A��A��A��A��A��A��mA��#A��
A�ĜA�`BA��RA�1'A��PA��A�r�A�=qA�XA�&�A���A�oA�/A��;A�-A��PA���A��yA���A�VA��`A��9A�33A�=qA���A�v�A�%A���A���A���A�M�A���A�ZA�n�A�1'A�n�A��
A�7LA�ĜA�"�A���A� �A���A�A���A�l�A�JA��9A��A��A�&�A�$�A�S�A�ffA��A��;A�p�A�1'A��A���A�XA�A���A��FA��RA�z�A�E�A��A�O�A�`BA��yA��A�jA��A��PA�$�A��7A��A�S�A��A���A�A�A�ĜA��\A�?}A��`A�hsAp�A}�hA{�Az�jAz�AxE�Av��AvZAt�Ar��Ar1Aq��Aq�Ao�FAn  AmAlZAk�Akl�Aj�`Ajn�Aj�Ai�^Agp�Ae��Ad�AcC�AbbNAa�A`E�A^��A]�
A]
=A\�HA\$�AZ��AZ$�AX��AW�AVjAU�
AT�AR�jAQ�FAQx�AQ"�AP�\AO�
AN�!ANQ�ANbAM��AMXAL�ALJAKVAI\)AH��AH�DAH�uAH��AHZAG�TAGAGC�AF~�AE��ADM�AC�AB�jAA��AA�A@�`A@��A@ �A?��A?C�A=ƨA=�A<��A<�+A;��A:JA7��A6�`A65?A5;dA4ZA3\)A2JA1�A0�yA/��A.�/A.jA-��A,��A+��A*bNA)�A';dA&$�A%XA#33A ��A   A�FA�7AbNAA�A��A7LAI�AS�A&�A��A�A��A��Az�AS�A��A=qA{A��AdZAA�At�A�A��AZAQ�A9XA�-A�9AhsA��Az�AJAt�AVA
�9A
jA
 �A	�A	��A�/AjA1A��A�yAbA�PA��A�
A(�A|�A/A ��A {@�J@���@�Z@�A�@��P@��R@�/@�dZ@��7@�+@�ff@�?}@�w@��@�\@�/@���@��@��@�hs@�V@�Ĝ@�@��@�@�^@��`@�Q�@�o@�5?@�Ĝ@�
=@��T@ݙ�@۝�@��/@�;d@�o@ְ!@�E�@���@ԣ�@�;d@Ͼw@�$�@�`B@̣�@�b@˅@��H@�$�@�7L@���@��@Ǖ�@�"�@���@ċD@�"�@�V@� �@�C�@��#@�?}@��`@�bN@��@���@��@���@�9X@�@�`B@��-@��@��9@�r�@��m@���@�?}@��P@��@���@��\@�n�@�E�@���@��-@�`B@�K�@��@�1'@���@�|�@��@���@�=q@��-@��@���@���@�(�@���@��H@���@�=q@�X@�%@�Ĝ@��j@�\)@�ȴ@��!@�-@��h@�G�@��@��;@��F@�|�@�K�@�
=@��y@���@�ff@�=q@�J@���@�@��7@�`B@�X@�&�@�z�@���@���@�~�@�ff@�-@���@�X@�7L@��@�Ĝ@�r�@� �@��@��m@��;@���@��@��h@�z�@��;@��@�C�@�o@�;d@�
=@���@�-@��h@�O�@�&�@�%@��/@��9@���@���@��@�j@�Z@�9X@��w@�@���@�~�@�M�@�=q@�J@�p�@���@�Q�@��@�\)@�@��R@�v�@��@�@��@��h@�p�@�?}@���@��`@�I�@��@K�@~�@~��@~�+@}�T@|�j@|Z@|�@{dZ@{o@z��@zM�@zJ@y�#@y��@yhs@y%@x�9@x1'@x  @w��@w�P@w+@v$�@u@u�h@t��@t��@t�@s��@sS�@s33@s@r��@r��@r^5@r�@q��@q�^@p��@pQ�@pA�@pA�@pA�@pb@o�;@o�P@o�@n�@n��@nv�@nV@n5?@m�-@l�j@k��@kS�@j�!@jn�@jn�@j^5@j=q@j=q@jM�@j=q@jJ@i�@i�^@i��@i�7@h��@h �@g�@g��@g\)@g
=@f�y@f�R@fff@f5?@f$�@f{@f{@e�T@e��@e�@e`B@d�@d��@d��@c��@b~�@a��@a�#@a�^@ahs@`�`@`Ĝ@`�u@`�@`A�@` �@_�@_�@_l�@_�@^v�@^@]�h@]`B@]?}@]/@]V@\�/@\z�@[ƨ@[��@[C�@[o@[@Z�H@Z��@Z�\@Zn�@ZM�@ZJ@Y��@X�`@X��@X�9@XbN@W�@W��@W��@W�@Wl�@W+@V��@V�R@U��@U�@UO�@U/@U/@UV@T�@T�j@T�j@T��@Tj@T1@Sƨ@S�@St�@St�@S33@S@R��@R��@R-@Q��@Q��@Q��@QX@Q&�@Q%@P��@Q%@P��@P�u@P �@O�;@O�@Ol�@O+@O�@N��@N�y@N��@NE�@M`B@L�/@Lz�@L(�@Kƨ@K�@J��@J~�@J^5@J-@I�#@I��@Ihs@IX@I&�@I�@I%@I%@H��@H��@H��@Hr�@G�@G�@G;d@G�@F�R@F�+@FE�@F{@E@D�@DI�@D�@D1@C�@CS�@C33@C"�@Co@Co@Co@Co@Co@B�H@BM�@BJ@A�@A��@Ahs@A7L@A�@@��@@��@@�9@@��@@�u@@r�@@b@?��@?�@>��@>$�@=��@=�h@=p�@=O�@=V@<��@<(�@;�
@;ƨ@;�
@;ƨ@;��@;S�@;@:��@:��@:��@:�\@:~�@:^5@9��@9�@8�`@8Q�@7�w@7l�@7�@6�y@6ȴ@6ȴ@6�R@6�+@65?@5�T@5�h@5�@5`B@5/@5�@4��@4�/@4��@4z�@4j@4I�@4�@41@3��@3��@3��@3�m@3ƨ@3�F@3S�@2�@2��@2n�@2=q@2-@2�@2�@2�@2�@2�@1�@1��@17L@0��@0��@0�@0r�@0Q�@0A�@0A�@0b@0b@0  @0  @0  @/�;@/��@/�@/+@.�@.�+@.E�@-�T@-?}@,z�@,Z@,I�@,9X@,(�@,�@,1@+�m@+�
@+�
@+ƨ@+ƨ@+ƨ@+�F@+�F@+��@+33@+@*�!@*^5@)�@)�#@)�#@)��@)�7@)hs@)X@)7L@)%@(��@(A�@'�@'�w@'�@'��@'�P@'l�@'\)@&�y@&v�@&$�@&@%�T@%�@%/@$�@$��@$j@$I�@$(�@#��@#��@#t�@#33@#@"�!@"^5@!��@!��@!�^@!hs@!X@!7L@!&�@!7L@!�@ Ĝ@ r�@ A�@ A�@ b@�@|�@K�@�y@ȴ@�+@v�@v�@@��@@�-@��@�h@p�@O�@�@�/@Z@1@�
@��@t�@C�@"�@o@��@M�@�#@��@�7@x�@x�@7L@�@��@Ĝ@�u@r�@Q�@�@l�@;d@+@�@
=@
=@��@�y@�y@�@ȴ@�+@$�@�T@�h@p�@V@Z@1@��@�m@ƨ@�F@dZ@�H@�\@n�@=q@��@X@G�@&�@%@��@�`@��@Ĝ@�9@�u@bN@Q�@1'@ �@  @�;@�@��@|�@+@�@
=@ȴ@��@v�@E�@{@@�T@��@p�@?}@V@�@�/@��@��@��@Z@9X@(�@1@�m@ƨ@ƨ@�@C�@C�@"�@@
�H@
��@
�!@
�\@
n�@
^5@
M�@
-@	�@	�^@	x�@	hs@	G�@	7L@	�@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A��A��A��A��A��A��A��A��A��A��mA��#A��
A�ĜA�`BA��RA�1'A��PA��A�r�A�=qA�XA�&�A���A�oA�/A��;A�-A��PA���A��yA���A�VA��`A��9A�33A�=qA���A�v�A�%A���A���A���A�M�A���A�ZA�n�A�1'A�n�A��
A�7LA�ĜA�"�A���A� �A���A�A���A�l�A�JA��9A��A��A�&�A�$�A�S�A�ffA��A��;A�p�A�1'A��A���A�XA�A���A��FA��RA�z�A�E�A��A�O�A�`BA��yA��A�jA��A��PA�$�A��7A��A�S�A��A���A�A�A�ĜA��\A�?}A��`A�hsAp�A}�hA{�Az�jAz�AxE�Av��AvZAt�Ar��Ar1Aq��Aq�Ao�FAn  AmAlZAk�Akl�Aj�`Ajn�Aj�Ai�^Agp�Ae��Ad�AcC�AbbNAa�A`E�A^��A]�
A]
=A\�HA\$�AZ��AZ$�AX��AW�AVjAU�
AT�AR�jAQ�FAQx�AQ"�AP�\AO�
AN�!ANQ�ANbAM��AMXAL�ALJAKVAI\)AH��AH�DAH�uAH��AHZAG�TAGAGC�AF~�AE��ADM�AC�AB�jAA��AA�A@�`A@��A@ �A?��A?C�A=ƨA=�A<��A<�+A;��A:JA7��A6�`A65?A5;dA4ZA3\)A2JA1�A0�yA/��A.�/A.jA-��A,��A+��A*bNA)�A';dA&$�A%XA#33A ��A   A�FA�7AbNAA�A��A7LAI�AS�A&�A��A�A��A��Az�AS�A��A=qA{A��AdZAA�At�A�A��AZAQ�A9XA�-A�9AhsA��Az�AJAt�AVA
�9A
jA
 �A	�A	��A�/AjA1A��A�yAbA�PA��A�
A(�A|�A/A ��A {@�J@���@�Z@�A�@��P@��R@�/@�dZ@��7@�+@�ff@�?}@�w@��@�\@�/@���@��@��@�hs@�V@�Ĝ@�@��@�@�^@��`@�Q�@�o@�5?@�Ĝ@�
=@��T@ݙ�@۝�@��/@�;d@�o@ְ!@�E�@���@ԣ�@�;d@Ͼw@�$�@�`B@̣�@�b@˅@��H@�$�@�7L@���@��@Ǖ�@�"�@���@ċD@�"�@�V@� �@�C�@��#@�?}@��`@�bN@��@���@��@���@�9X@�@�`B@��-@��@��9@�r�@��m@���@�?}@��P@��@���@��\@�n�@�E�@���@��-@�`B@�K�@��@�1'@���@�|�@��@���@�=q@��-@��@���@���@�(�@���@��H@���@�=q@�X@�%@�Ĝ@��j@�\)@�ȴ@��!@�-@��h@�G�@��@��;@��F@�|�@�K�@�
=@��y@���@�ff@�=q@�J@���@�@��7@�`B@�X@�&�@�z�@���@���@�~�@�ff@�-@���@�X@�7L@��@�Ĝ@�r�@� �@��@��m@��;@���@��@��h@�z�@��;@��@�C�@�o@�;d@�
=@���@�-@��h@�O�@�&�@�%@��/@��9@���@���@��@�j@�Z@�9X@��w@�@���@�~�@�M�@�=q@�J@�p�@���@�Q�@��@�\)@�@��R@�v�@��@�@��@��h@�p�@�?}@���@��`@�I�@��@K�@~�@~��@~�+@}�T@|�j@|Z@|�@{dZ@{o@z��@zM�@zJ@y�#@y��@yhs@y%@x�9@x1'@x  @w��@w�P@w+@v$�@u@u�h@t��@t��@t�@s��@sS�@s33@s@r��@r��@r^5@r�@q��@q�^@p��@pQ�@pA�@pA�@pA�@pb@o�;@o�P@o�@n�@n��@nv�@nV@n5?@m�-@l�j@k��@kS�@j�!@jn�@jn�@j^5@j=q@j=q@jM�@j=q@jJ@i�@i�^@i��@i�7@h��@h �@g�@g��@g\)@g
=@f�y@f�R@fff@f5?@f$�@f{@f{@e�T@e��@e�@e`B@d�@d��@d��@c��@b~�@a��@a�#@a�^@ahs@`�`@`Ĝ@`�u@`�@`A�@` �@_�@_�@_l�@_�@^v�@^@]�h@]`B@]?}@]/@]V@\�/@\z�@[ƨ@[��@[C�@[o@[@Z�H@Z��@Z�\@Zn�@ZM�@ZJ@Y��@X�`@X��@X�9@XbN@W�@W��@W��@W�@Wl�@W+@V��@V�R@U��@U�@UO�@U/@U/@UV@T�@T�j@T�j@T��@Tj@T1@Sƨ@S�@St�@St�@S33@S@R��@R��@R-@Q��@Q��@Q��@QX@Q&�@Q%@P��@Q%@P��@P�u@P �@O�;@O�@Ol�@O+@O�@N��@N�y@N��@NE�@M`B@L�/@Lz�@L(�@Kƨ@K�@J��@J~�@J^5@J-@I�#@I��@Ihs@IX@I&�@I�@I%@I%@H��@H��@H��@Hr�@G�@G�@G;d@G�@F�R@F�+@FE�@F{@E@D�@DI�@D�@D1@C�@CS�@C33@C"�@Co@Co@Co@Co@Co@B�H@BM�@BJ@A�@A��@Ahs@A7L@A�@@��@@��@@�9@@��@@�u@@r�@@b@?��@?�@>��@>$�@=��@=�h@=p�@=O�@=V@<��@<(�@;�
@;ƨ@;�
@;ƨ@;��@;S�@;@:��@:��@:��@:�\@:~�@:^5@9��@9�@8�`@8Q�@7�w@7l�@7�@6�y@6ȴ@6ȴ@6�R@6�+@65?@5�T@5�h@5�@5`B@5/@5�@4��@4�/@4��@4z�@4j@4I�@4�@41@3��@3��@3��@3�m@3ƨ@3�F@3S�@2�@2��@2n�@2=q@2-@2�@2�@2�@2�@2�@1�@1��@17L@0��@0��@0�@0r�@0Q�@0A�@0A�@0b@0b@0  @0  @0  @/�;@/��@/�@/+@.�@.�+@.E�@-�T@-?}@,z�@,Z@,I�@,9X@,(�@,�@,1@+�m@+�
@+�
@+ƨ@+ƨ@+ƨ@+�F@+�F@+��@+33@+@*�!@*^5@)�@)�#@)�#@)��@)�7@)hs@)X@)7L@)%@(��@(A�@'�@'�w@'�@'��@'�P@'l�@'\)@&�y@&v�@&$�@&@%�T@%�@%/@$�@$��@$j@$I�@$(�@#��@#��@#t�@#33@#@"�!@"^5@!��@!��@!�^@!hs@!X@!7L@!&�@!7L@!�@ Ĝ@ r�@ A�@ A�@ b@�@|�@K�@�y@ȴ@�+@v�@v�@@��@@�-@��@�h@p�@O�@�@�/@Z@1@�
@��@t�@C�@"�@o@��@M�@�#@��@�7@x�@x�@7L@�@��@Ĝ@�u@r�@Q�@�@l�@;d@+@�@
=@
=@��@�y@�y@�@ȴ@�+@$�@�T@�h@p�@V@Z@1@��@�m@ƨ@�F@dZ@�H@�\@n�@=q@��@X@G�@&�@%@��@�`@��@Ĝ@�9@�u@bN@Q�@1'@ �@  @�;@�@��@|�@+@�@
=@ȴ@��@v�@E�@{@@�T@��@p�@?}@V@�@�/@��@��@��@Z@9X@(�@1@�m@ƨ@ƨ@�@C�@C�@"�@@
�H@
��@
�!@
�\@
n�@
^5@
M�@
-@	�@	�^@	x�@	hs@	G�@	7L@	�@	%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bk�Bk�Bl�Bk�Bk�Bk�Bk�Bk�Bl�Bl�Bk�Bl�Bl�Bl�Bk�B�PB��B�}B�jB�FB�'B��B��B�7Bz�B`BBT�BP�BE�B6FB,B&�B#�B�B�B�B�BJB��B�NB�B�}B�?B�!B�B��B��B��B��B�\B�JB�+B~�Bx�Bq�BiyBdZB_;BZBT�BO�BJ�B>wB?}B1'B�B
=B��B��B�B�ZB�;B�B��B��BȴBĜBB�!B��B�Bs�Bl�B_;BT�BO�BJ�BC�B;dB33B(�B�BB
�mB
�#B
�B
��B
��B
ǮB
��B
�RB
�B
��B
�PB
�B
~�B
t�B
iyB
e`B
]/B
M�B
D�B
A�B
<jB
33B
%�B
�B
�B
{B
bB
JB
1B
B
B	��B	�`B	�;B	��B	ɺB	ƨB	�wB	�LB	�!B	�B	��B	��B	��B	��B	��B	�PB	�B	� B	{�B	p�B	hsB	ffB	cTB	_;B	ZB	R�B	N�B	M�B	J�B	I�B	E�B	C�B	>wB	6FB	33B	49B	6FB	8RB	6FB	33B	33B	1'B	+B	(�B	�B	�B	%�B	�B	$�B	(�B	'�B	%�B	#�B	!�B	�B	{B	{B	uB	VB	B��B�B�B�B�B�`B�BB�)B�B�
B��B��B��BĜB�qB�!B��B��B�{B�\B�+B~�B|�B|�B{�Bz�Bw�Bv�Bt�Bq�Bn�BjBiyBhsBffBe`BdZBaHB^5B[#BZBYBXBW
BT�BR�BP�BO�BN�BM�BM�BL�BK�BH�BD�BC�BC�BB�BA�B@�B@�B?}B?}B?}B>wB=qB=qB<jB=qB=qB<jB:^B:^B6FB5?B33B2-B1'B/B.B-B-B,B+B)�B'�B&�B#�B"�B"�B#�B"�B!�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B!�B#�B$�B$�B&�B(�B(�B)�B)�B+B+B,B.B0!B2-B5?B<jB=qB=qB>wB>wB>wBD�BG�BH�BH�BI�BI�BJ�BJ�BK�BK�BR�BW
B_;BaHBaHBbNBcTBdZBffBiyBjBjBl�Bo�Bs�Bt�Bu�B{�B|�B}�B}�B�B�%B�%B�7B�JB�PB�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�9B�9B�?B�LB�XB�^B�dB�qB�}B��B��B��B��BBǮB��B��B�B�#B�/B�HB�TB�`B�mB�yB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	+B	JB	PB	\B	�B	�B	�B	�B	 �B	!�B	"�B	%�B	'�B	)�B	-B	-B	1'B	49B	6FB	8RB	9XB	9XB	<jB	A�B	C�B	D�B	F�B	H�B	I�B	J�B	K�B	L�B	M�B	N�B	O�B	P�B	S�B	S�B	W
B	W
B	YB	]/B	_;B	`BB	cTB	dZB	ffB	hsB	iyB	jB	l�B	m�B	o�B	p�B	r�B	s�B	s�B	v�B	z�B	{�B	{�B	|�B	}�B	}�B	� B	�B	�B	�B	�B	�B	�B	�+B	�JB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�RB	�XB	�^B	�^B	�dB	�qB	�qB	�wB	�wB	��B	��B	B	ÖB	ÖB	ŢB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�#B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
1B
	7B

=B
DB
DB
JB
VB
\B
\B
bB
bB
oB
oB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
%�B
&�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
,B
,B
,B
-B
-B
-B
.B
.B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
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
5?B
5?B
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
?}B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
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
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
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
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
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
XB
XB
XB
XB
XB
XB
XB
XB
XB
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
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
`BB
`BB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
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
e`B
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bk�Bk�Bl�Bk�Bk�Bk�Bk�Bk�Bl�Bl�Bk�Bl�BmBn/Bp�B��B��B��B��B��B��B�8B��B�6B�BbNBV�BT�BIRB:B.B)�B&�B�BxBWB1B�B�"B�BݲBB�2B�vB��B�B�B�HB�eB��B��B�KB��BzBr�Bj�Be�B_�BZ�BU�BQ BL�B@BB�B3MBB�B��B��B��B��B��B��BյBϫB�RBňB�B�B�)B�{BuBn}B`\BU�BP�BK�BD�B<�B4�B+�B�BzB
�DB
��B
�$B
�}B
̘B
ȴB
��B
�*B
�/B
�~B
��B
�MB
� B
vzB
j�B
gRB
_!B
N�B
EmB
B�B
>BB
5B
'B
�B
+B
MB
4B
B
	B
9B
�B	��B	��B	��B	�&B	��B	ȀB	�iB	�RB	�B	��B	�6B	�mB	��B	�]B	�EB	��B	�9B	��B	}�B	q�B	h�B	gB	d&B	`\B	[qB	S�B	OvB	N�B	K^B	J�B	F�B	EB	@4B	7B	3�B	4TB	6�B	8�B	6�B	3�B	4B	2GB	,qB	*B	 �B	 �B	&�B	�B	%�B	)yB	(�B	&�B	$�B	#nB	�B	B	MB	B	�B	aB�dB��B�B��B��B�B�HB�IB�qB�_B��B��B�~B�B�HB�B�*B�\B�SB�TB��B�B}qB}�B}qB{�Bx�BxBv�Br�Bo�BkBj0Bi_BgBfLBe�Bb�B_!B[�BZ�BY�BX�BX�BVBS�BQ�BPHBOBNpBN�BNVBMjBI�BEBDgBD�BC-BB'BA BAB@B@OB@�B?HB>(B>]B=�B>�B>]B=�B<6B<jB72B5�B4B3hB2|B0B.}B-]B-�B,�B,=B+QB)yB(sB$�B#�B#�B$tB#�B"�B#TB!|B 'B BB!B�B;B�B�BjBIB�B~B�B�B�BqB#BxB�B�B�BB1B�BB�B�BCB]BCB/BdBjBpB;BVB BB vB �B"�B#B%FB%�B%�B'�B)�B)yB*eB*eB+kB+�B-B/OB1AB3�B7LB<�B=�B=�B>�B?HB@BE�BH1BIBIBI�BJ	BK)BK)BL�BMjBTFBW�B_�Ba|Ba�Bb�Bc�Bd�Bf�Bi�Bj�Bj�Bm)Bp;BtBu%Bv`B|PB}<B~]B~�B��B�tB��B��B��B��B�B��B��B��B��B��B��B�!B�B��B� B�B�,B�2B�B�2B��B��B��B�|B��B��B��B��B��B��B��B��B��B��B��B��B��B�aBȴBΥBՁB�eB�qB�~B�bB�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�%B�RB�<B�HB	 OB	AB	[B	�B	�B	�B	�B	�B	�B	�B	�B	B	 �B	"B	#:B	&2B	($B	*KB	-]B	-�B	1�B	4�B	6zB	8lB	9�B	9�B	<�B	A�B	C�B	EB	F�B	IB	I�B	J�B	K�B	MB	NB	OB	P.B	Q4B	TB	T,B	W$B	WYB	Y�B	]dB	_VB	`�B	c�B	d�B	f�B	h�B	i�B	j�B	l�B	m�B	o�B	p�B	r�B	tB	tB	wB	z�B	{�B	|B	}"B	~B	~(B	�OB	�AB	�-B	�3B	�3B	�SB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�
B	�B	�B	�QB	�IB	�OB	��B	��B	��B	�rB	��B	��B	��B	��B	��B	��B	��B	��B	��B	ªB	��B	��B	��B	��B	�	B	��B	��B	�B	�B	�B	�B	�B	�B	� B	�&B	�B	�2B	�B	�9B	�$B	�?B	�+B	�eB	�WB	�jB	�OB	�pB	�\B	�\B	�bB	�|B	�hB	�B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�$B	��B	�B	�B	�B	�(B	�B	��B	�B	�B	�HB
 OB
 B
 B
AB
GB
B
MB
MB
3B
gB
�B
fB
	�B

rB
xB
xB
�B
pB
vB
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
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
#B
"�B
"�B
$B
$B
#�B
#�B
$B
%B
%B
%B
%B
%,B
&2B
'B
(
B
)B
)B
)B
)B
*KB
*KB
+6B
+B
+B
+B
+6B
+6B
,"B
,=B
,"B
,"B
,"B
-)B
-CB
-]B
.cB
./B
.IB
/iB
/5B
0;B
0UB
0UB
1AB
1AB
1[B
1[B
1AB
2GB
2aB
2GB
3MB
3MB
3hB
4nB
4nB
4nB
4nB
5tB
5ZB
5ZB
5ZB
5ZB
5ZB
5ZB
5tB
5ZB
5�B
6zB
6zB
7�B
7�B
7�B
8RB
8lB
8RB
8lB
8lB
8lB
8lB
8�B
9�B
:�B
:�B
:xB
:�B
:�B
;B
;�B
;B
;dB
;B
;B
;B
<�B
<�B
<�B
=�B
>�B
>�B
>�B
?�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
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
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
NB
M�B
N�B
N�B
OB
N�B
OB
O�B
O�B
O�B
PB
O�B
PB
Q B
Q B
Q B
QB
QB
RB
R B
R B
R�B
S&B
S&B
S&B
TB
S�B
TB
TB
T,B
TB
TB
TB
UMB
U2B
V9B
V9B
V9B
V9B
V9B
W$B
W?B
W?B
X_B
XEB
X+B
XB
X+B
XEB
X+B
XEB
XEB
YKB
YKB
Y1B
YKB
ZQB
Z7B
[=B
[=B
[#B
[=B
[=B
[#B
[=B
[=B
[=B
[WB
[qB
\]B
\CB
\]B
\]B
]dB
^jB
^5B
^OB
^jB
^jB
^OB
_�B
`\B
`vB
`vB
`�B
a|B
bNB
bhB
bhB
bhB
bNB
bhB
bhB
bNB
b�B
c�B
cnB
c�B
cnB
c�B
c�B
dtB
dtB
d�B
dtB
ezB
dtB
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
hsB
h�B
h�B
h�B
hsB
h�B
h�B
i�B
i�B
i�B
i�B
iyB
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.12(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201606210033502016062100335020160621003350201806221209422018062212094220180622120942201804050401542018040504015420180405040154  JA  ARFMdecpA19c                                                                20160624183528  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160624094826  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160624094826  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160624094827  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160624094828  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160624094828  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160624094828  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160624094828  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20160624094828                      G�O�G�O�G�O�                JA  ARUP                                                                        20160624102536                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160617153524  CV  JULD            G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20160617153524  CV  JULD_LOCATION   G�O�G�O�F���                JM  ARGQJMQC2.0                                                                 20160617153524  CV  LATITUDE        G�O�G�O�A�X                JM  ARGQJMQC2.0                                                                 20160617153524  CV  LONGITUDE       G�O�G�O��#e                JM  ARCAJMQC2.0                                                                 20160620153350  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160620153350  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404190154  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622030942  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115101517                      G�O�G�O�G�O�                