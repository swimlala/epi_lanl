CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-18T00:35:13Z creation;2018-08-18T00:35:19Z conversion to V3.1;2019-12-19T07:34:53Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180818003513  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0576_271                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�zs�-� 1   @�zt��>�@9����o�da��v�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D���D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�	�D�&f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @1�@xQ�@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCiǮCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���D xRD �RDxRD�RDxRD�RDxRD�RDxRD��DxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�x�D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��\D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�x�D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D���D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�\Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��D�"�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A��A�ffA�jA�l�A�-AθRA�ffA�{A���A��/A�r�A�?}A�
=A�A�hsA�"�A�VA���A˙�A��;A��A��A��A��A�33A��!A�-A��A��A��#A�M�A��A��A�r�A�oA��;A���A��A�?}A��A���A�/A���A��A�VA���A��A��A���A���A���A��uA�  A���A��A�dZA���A���A�;dA�A��\A��;A���A�$�A�9XA�?}A���A�;dA��A�p�A��A�7LA�x�A��A���A�r�A���A�l�A�I�A�9XA�/A�{A��A�ȴA��wA�+A��/A��
A���A��A}`BA|bNA{��A{dZAz��AzVAy+AxJAu/Aqx�AoK�AnA�Al��Akp�Aj�Aj�uAj{Ai�Ah�!Ah=qAg�AfAe
=Ac�Ab�/A`��A_ƨA_�A]�hA\Q�A\{A[�#A[A[��A[33AY/AW+AU�AUhsAT��AS��AS
=AR��AR$�AQ�TAQAP�/AO�AO7LAN��AMVAK�mAK`BAK+AJ��AJĜAJv�AJ �AI�^AH�/AH1'AG��AF��AD��AC&�AB�ABbNAA+A?�
A>�/A>��A>�\A>$�A=�7A<�uA;t�A:�`A:bNA8�/A6�HA5�;A4�A4(�A3XA2�A0��A/�#A.��A-��A,�\A+�TA+|�A*��A*��A*ffA*�A)+A(  A'7LA%��A$A�A"�/A"5?A!��A!p�A!&�A 9XA�PA�!A=qA�7A�yA~�A�AS�AjAC�AbA�jA��Ar�AJA�AO�A�!AJAhsA��A��A�TAhsAĜA1'A\)A
(�A	oA1'A��AdZAoA��A1A�AM�A��A`BA�+A33A Z@�@�Z@�b@�S�@�E�@���@�x�@�7L@��/@���@�~�@��@���@� �@��@�@�%@� �@�@�=q@��`@�$�@�&�@�j@��H@�E�@�@�j@��@�+@���@�=q@���@�Ĝ@ݩ�@�&�@�z�@��H@�X@ؓu@��@�\)@�@�V@�G�@Ӿw@�"�@с@϶F@��T@�?}@�  @ʸR@���@ɉ7@��/@ǥ�@���@�~�@ř�@�%@���@���@ă@î@�K�@���@�@���@��-@�7L@��P@�$�@�p�@��u@��
@���@�X@�b@�;d@�+@�+@�"�@�ȴ@�J@�@��#@���@�  @�n�@�p�@��9@� �@�\)@�M�@���@��`@�z�@���@�C�@���@�@��h@��@��;@�K�@�l�@���@�G�@��@��@�Z@��@���@��@�X@�hs@�j@���@���@��^@�/@��/@�j@�S�@�V@�{@��#@���@�x�@�hs@�X@�O�@�7L@��@���@���@��!@�{@��@��-@�x�@�G�@��`@�bN@�K�@��R@�ff@�$�@��#@���@�&�@��9@��u@�9X@�|�@�l�@�S�@���@�ȴ@���@�$�@�x�@��/@���@�bN@�  @�ƨ@���@�
=@��@��T@�O�@��9@�r�@���@�C�@�"�@���@�ff@��@��@�&�@�Ĝ@�bN@��@�ƨ@���@�t�@�@��@��R@��!@���@�v�@�v�@�^5@�=q@�$�@�@��@�X@�%@��D@� �@�@��@|�@;d@~�y@~5?@}�@}V@|��@|�@|�@{�
@{��@{C�@z�!@zJ@y�@y��@y��@y��@yhs@y%@xA�@wl�@w+@v�y@vV@vE�@v5?@v$�@v5?@v$�@v$�@v{@v$�@v$�@v@u@u@u�@u�@t��@tI�@t(�@t9X@s�m@sƨ@s�@r�@rn�@q��@qX@p��@pbN@pb@o��@n��@n��@nE�@m��@m�@l��@l��@l�j@lZ@l�@k�m@k�
@kƨ@kƨ@kdZ@k"�@jM�@i��@i�@ihs@h��@hĜ@h�9@hĜ@hA�@g��@g+@f�y@f��@f5?@e�@e�-@e`B@d��@dj@d�@c��@cC�@co@b�H@b�!@b-@a�^@aG�@`��@`bN@`  @_|�@_
=@^ff@^V@^5?@]�T@]�-@]/@\�/@\�@\(�@[dZ@["�@[o@Z�@Z��@Z�\@Y��@Y��@Yx�@Y7L@Y�@X��@XbN@X1'@W�;@W�w@W��@W+@V�R@VE�@U�@U@U��@U�@U�@U`B@U/@T�/@T(�@S�m@S�F@SC�@R��@RM�@QX@Q%@P�9@PbN@PQ�@O�;@O
=@Nȴ@Nv�@M�@M��@M��@M��@M?}@L�j@L(�@K�m@Kt�@K"�@J�@J�!@J~�@J=q@JJ@I��@I7L@H��@HbN@H1'@Hb@Hb@G�;@G��@G+@Fȴ@Fv�@FV@F$�@F$�@F{@E�h@E�@D�@Dz�@D1@Cƨ@Ct�@B��@B^5@A��@A�#@A�#@A��@A�7@Ahs@AG�@@�`@@�u@@A�@?�;@?�P@?;d@>�R@>@=�-@=�h@=O�@<�@<�j@<Z@<(�@;��@;��@;�@;"�@:�@:��@9�#@9G�@97L@8�`@8��@8��@8�@81'@7�w@7|�@7\)@7�@7
=@6V@5@5�@5p�@4��@4�@4��@4�@3�F@3��@3�@3"�@2�@2�!@2-@1�@1�7@1X@17L@1&�@0��@0�@0bN@0A�@0b@/�@/�w@/�@/��@/l�@/\)@/�@/
=@.��@.ȴ@-�@-p�@,��@,�j@,�D@,(�@+ƨ@+�@+@*�@*�H@*��@*�!@*�!@*�\@*=q@*J@)�#@)hs@(��@(�u@(bN@(bN@(A�@( �@(  @'��@'|�@';d@'+@'�@'
=@&V@%��@%/@%V@$��@$j@$(�@$�@#��@#S�@#33@#o@#o@#@"��@"��@"M�@!�@!X@!�@ �`@ Ĝ@ ��@ Ĝ@ �9@ �u@ bN@ 1'@ b@�w@l�@�@�@ȴ@��@�+@v�@ff@E�@$�@�@��@�-@�@O�@?}@V@�/@��@��@j@(�@�F@dZ@C�@o@n�@�@��@x�@X@G�@7L@&�@��@��@�9@�u@�@�u@�u@�@r�@bN@bN@A�@  @�;@�w@��@l�@K�@�@�y@�@��@v�@V@@@�h@�@�@O�@�@�@�@I�@�@1@��@��@S�@33@��@n�@-@��@��@x�@G�@7L@%@��@��@r�@Q�@1'@�;@�@\)@+@�y@�y@�@�@�@ȴ@ȴ@��@v�@5?@$�@��@�@p�@O�@?}@?}@/@�@��@�@��@z�@j@I�@�@��@��@�m@�
@�F@�F@��@�@t�@S�@o@
�@
�\@
-@
�@	�@	��@	��@	x�@	hs@	hs@	7L@��@��@�u@r�@�@�@��@|�@K�@K�@;d@+@
=@�y@�@�R@�R@��@E�@@�T@��@@�@O�@/@�@�j@��@�D@Z@�@1@1@ƨ@�@o@�\@�\@~�@~�@~�@n�@=q@�@J@��@��@�7@G�@%@ �`@ ��@ Ĝ@ �9@ �u@ r�@ bN@ A�@ 1'@  �@  �?��;?���?�\)?�;d?��?��?���?���?���?�5??�{?��?��h?�O�?�/?��?���?���?��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A��A�ffA�jA�l�A�-AθRA�ffA�{A���A��/A�r�A�?}A�
=A�A�hsA�"�A�VA���A˙�A��;A��A��A��A��A�33A��!A�-A��A��A��#A�M�A��A��A�r�A�oA��;A���A��A�?}A��A���A�/A���A��A�VA���A��A��A���A���A���A��uA�  A���A��A�dZA���A���A�;dA�A��\A��;A���A�$�A�9XA�?}A���A�;dA��A�p�A��A�7LA�x�A��A���A�r�A���A�l�A�I�A�9XA�/A�{A��A�ȴA��wA�+A��/G�O�G�O�A��A}`BA|bNA{��A{dZAz��AzVAy+AxJAu/Aqx�AoK�AnA�Al��Akp�Aj�Aj�uAj{Ai�Ah�!Ah=qAg�AfAe
=Ac�Ab�/A`��A_ƨA_�A]�hA\Q�A\{A[�#A[A[��A[33AY/AW+AU�AUhsAT��AS��AS
=AR��AR$�AQ�TAQAP�/AO�AO7LAN��AMVAK�mAK`BAK+AJ��AJĜAJv�AJ �AI�^AH�/AH1'AG��AF��AD��AC&�AB�ABbNAA+A?�
A>�/A>��A>�\A>$�A=�7A<�uA;t�A:�`A:bNA8�/A6�HA5�;A4�A4(�A3XA2�A0��A/�#A.��A-��A,�\A+�TA+|�A*��A*��A*ffA*�A)+A(  A'7LA%��A$A�A"�/A"5?A!��A!p�A!&�A 9XA�PA�!A=qA�7A�yA~�A�AS�AjAC�AbA�jA��Ar�AJA�AO�A�!AJAhsA��A��A�TAhsAĜA1'A\)A
(�A	oA1'A��AdZAoA��A1A�AM�A��A`BA�+A33A Z@�@�Z@�b@�S�@�E�@���@�x�@�7L@��/@���@�~�@��@���@� �@��@�@�%@� �@�@�=q@��`@�$�@�&�@�j@��H@�E�@�@�j@��@�+@���@�=q@���@�Ĝ@ݩ�@�&�@�z�@��H@�X@ؓu@��@�\)@�@�V@�G�@Ӿw@�"�@с@϶F@��T@�?}@�  @ʸR@���@ɉ7@��/@ǥ�@���@�~�@ř�@�%@���@���@ă@î@�K�@���@�@���@��-@�7L@��P@�$�@�p�@��u@��
@���@�X@�b@�;d@�+@�+@�"�@�ȴ@�J@�@��#@���@�  @�n�@�p�@��9@� �@�\)@�M�@���@��`@�z�@���@�C�@���@�@��h@��@��;@�K�@�l�@���@�G�@��@��@�Z@��@���@��@�X@�hs@�j@���@���@��^@�/@��/@�j@�S�@�V@�{@��#@���@�x�@�hs@�X@�O�@�7L@��@���@���@��!@�{@��@��-@�x�@�G�@��`@�bN@�K�@��R@�ff@�$�@��#@���@�&�@��9@��u@�9X@�|�@�l�@�S�@���@�ȴ@���@�$�@�x�@��/@���@�bN@�  @�ƨ@���@�
=@��@��T@�O�@��9@�r�@���@�C�@�"�@���@�ff@��@��@�&�@�Ĝ@�bN@��@�ƨ@���@�t�@�@��@��R@��!@���@�v�@�v�@�^5@�=q@�$�@�@��@�X@�%@��D@� �@�@��@|�@;d@~�y@~5?@}�@}V@|��@|�@|�@{�
@{��@{C�@z�!@zJ@y�@y��@y��@y��@yhs@y%@xA�@wl�@w+@v�y@vV@vE�@v5?@v$�@v5?@v$�@v$�@v{@v$�@v$�@v@u@u@u�@u�@t��@tI�@t(�@t9X@s�m@sƨ@s�@r�@rn�@q��@qX@p��@pbN@pb@o��@n��@n��@nE�@m��@m�@l��@l��@l�j@lZ@l�@k�m@k�
@kƨ@kƨ@kdZ@k"�@jM�@i��@i�@ihs@h��@hĜ@h�9@hĜ@hA�@g��@g+@f�y@f��@f5?@e�@e�-@e`B@d��@dj@d�@c��@cC�@co@b�H@b�!@b-@a�^@aG�@`��@`bN@`  @_|�@_
=@^ff@^V@^5?@]�T@]�-@]/@\�/@\�@\(�@[dZ@["�@[o@Z�@Z��@Z�\@Y��@Y��@Yx�@Y7L@Y�@X��@XbN@X1'@W�;@W�w@W��@W+@V�R@VE�@U�@U@U��@U�@U�@U`B@U/@T�/@T(�@S�m@S�F@SC�@R��@RM�@QX@Q%@P�9@PbN@PQ�@O�;@O
=@Nȴ@Nv�@M�@M��@M��@M��@M?}@L�j@L(�@K�m@Kt�@K"�@J�@J�!@J~�@J=q@JJ@I��@I7L@H��@HbN@H1'@Hb@Hb@G�;@G��@G+@Fȴ@Fv�@FV@F$�@F$�@F{@E�h@E�@D�@Dz�@D1@Cƨ@Ct�@B��@B^5@A��@A�#@A�#@A��@A�7@Ahs@AG�@@�`@@�u@@A�@?�;@?�P@?;d@>�R@>@=�-@=�h@=O�@<�@<�j@<Z@<(�@;��@;��@;�@;"�@:�@:��@9�#@9G�@97L@8�`@8��@8��@8�@81'@7�w@7|�@7\)@7�@7
=@6V@5@5�@5p�@4��@4�@4��@4�@3�F@3��@3�@3"�@2�@2�!@2-@1�@1�7@1X@17L@1&�@0��@0�@0bN@0A�@0b@/�@/�w@/�@/��@/l�@/\)@/�@/
=@.��@.ȴ@-�@-p�@,��@,�j@,�D@,(�@+ƨ@+�@+@*�@*�H@*��@*�!@*�!@*�\@*=q@*J@)�#@)hs@(��@(�u@(bN@(bN@(A�@( �@(  @'��@'|�@';d@'+@'�@'
=@&V@%��@%/@%V@$��@$j@$(�@$�@#��@#S�@#33@#o@#o@#@"��@"��@"M�@!�@!X@!�@ �`@ Ĝ@ ��@ Ĝ@ �9@ �u@ bN@ 1'@ b@�w@l�@�@�@ȴ@��@�+@v�@ff@E�@$�@�@��@�-@�@O�@?}@V@�/@��@��@j@(�@�F@dZ@C�@o@n�@�@��@x�@X@G�@7L@&�@��@��@�9@�u@�@�u@�u@�@r�@bN@bN@A�@  @�;@�w@��@l�@K�@�@�y@�@��@v�@V@@@�h@�@�@O�@�@�@�@I�@�@1@��@��@S�@33@��@n�@-@��@��@x�@G�@7L@%@��@��@r�@Q�@1'@�;@�@\)@+@�y@�y@�@�@�@ȴ@ȴ@��@v�@5?@$�@��@�@p�@O�@?}@?}@/@�@��@�@��@z�@j@I�@�@��@��@�m@�
@�F@�F@��@�@t�@S�@o@
�@
�\@
-@
�@	�@	��@	��@	x�@	hs@	hs@	7L@��@��@�u@r�@�@�@��@|�@K�@K�@;d@+@
=@�y@�@�R@�R@��@E�@@�T@��@@�@O�@/@�@�j@��@�D@Z@�@1@1@ƨ@�@o@�\@�\@~�@~�@~�@n�@=q@�@J@��@��@�7@G�@%@ �`@ ��@ Ĝ@ �9@ �u@ r�@ bN@ A�@ 1'@  �@  �?��;?���?�\)?�;d?��?��?���?���?���?�5??�{?��?��h?�O�?�/?��?���?���?��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BffB_;BO�BF�BVBR�BVBW
B[#BYBS�B^5B_;BZBW
BYBZBVB@�BJBÖBE�B9XBQ�BA�B\)B��B��B��B�BXB{�B�Bp�BYB� B}�B{�Bv�BffBB�B=qB,BE�B6FB'�B&�B)�B�B	7B�B�B�B�yB��BƨBÖB�-B�B�B��B��B�uB�%Bs�BcTB]/BVBA�B:^B0!B�BJB
�B
�fB
�`B
�HB
�)B
�NB
�BB
�)B
��B
B
�B
��B
��B
��B
��B
�B
L�B
:^B
ZB
`BB
W
B
P�B
F�B
33B
#�B
B	�mB	�B	��B	�B	�B	�B	�B	�B	�;B	�HB	�)B	��B	��B	ǮB	��B	�?B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�DB	r�B	dZB	m�B	q�B	m�B	iyB	jB	m�B	hsB	hsB	e`B	\)B	L�B	S�B	N�B	>wB	=qB	I�B	M�B	M�B	J�B	F�B	B�B	=qB	49B	1'B	-B	!�B	DB	PB	�B	�B	
=B	+B	B	bB	DB	B��B�B�B�B�`B��BƨB��B��BǮB��B�3B�'B��B��B��B��B��B��B��B��B��B��B�+Bx�By�Bp�Be`BjBx�Bz�Bx�Bt�BiyBjBgmBl�BiyBe`Be`B_;BVBL�BA�BC�B;dB>wB?}BL�BN�BP�BO�BN�BM�BJ�B?}B49BI�BG�BC�B?}B5?B8RB7LB>wB;dB;dB7LB33B0!B&�B,B+B"�B�B�B�B�B-B(�B&�B)�B/B,B'�B"�B�B%�B�B �B�B$�B"�B�B�B�B{B	7B�B�B�B�B�B�B�B�B �B�B�B�B+B �B�B�B�B!�B$�B"�B$�B �B�B�B�B�B�B�B!�B �B �B&�B(�B%�B#�B&�B,B)�B.B2-B49B2-B.B2-B2-B2-B/B1'B-B#�B'�B0!B/B.B-B)�B2-B9XBE�BE�BC�BA�B?}BE�BC�B>wB49B0!B9XB;dB=qB=qB>wBC�BF�BK�BK�BL�BL�BM�BT�BR�BM�BXB^5BXBYBcTBffBffBcTBdZBm�Bn�Bv�Bp�Br�Bt�By�B� B�B�B�B�B�oB�{B��B��B��B��B��B��B��B��B��B��B��B�!B�B�!B�!B�B�B�B�B�?B�^B�jB�qB�qB��BǮBŢBĜB��B��B��B��B��B��B��B�
B�5B�;B�BB�TB�TB�HB�;B�B�B�B��B��B��B	B	B	B	B		7B	JB	\B	oB	�B	�B	�B	!�B	"�B	)�B	,B	.B	.B	/B	2-B	2-B	33B	49B	5?B	5?B	33B	9XB	:^B	>wB	A�B	D�B	H�B	I�B	I�B	I�B	L�B	O�B	R�B	S�B	Q�B	YB	\)B	]/B	`BB	cTB	hsB	iyB	jB	jB	iyB	iyB	iyB	l�B	q�B	s�B	t�B	x�B	y�B	y�B	z�B	z�B	{�B	{�B	|�B	}�B	|�B	|�B	}�B	}�B	~�B	~�B	�B	�B	�B	�B	�+B	�+B	�%B	�7B	�=B	�PB	�\B	�hB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	�'B	�3B	�?B	�9B	�FB	�^B	�qB	�}B	��B	ÖB	ĜB	ĜB	ÖB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�/B	�)B	�)B	�/B	�)B	�/B	�;B	�5B	�5B	�TB	�ZB	�ZB	�ZB	�TB	�TB	�`B	�mB	�mB	�sB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B
  B
B
B
B
B
+B
+B
+B
+B
1B
+B
1B
	7B

=B
JB
PB
PB
JB
JB
JB
PB
VB
bB
bB
bB
bB
VB
\B
hB
bB
hB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
$�B
$�B
$�B
&�B
%�B
$�B
$�B
%�B
&�B
&�B
&�B
$�B
&�B
(�B
)�B
(�B
+B
)�B
)�B
+B
-B
-B
,B
-B
-B
,B
.B
.B
/B
1'B
0!B
0!B
/B
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
33B
33B
2-B
/B
2-B
49B
5?B
6FB
5?B
6FB
7LB
7LB
9XB
9XB
:^B
9XB
:^B
9XB
8RB
9XB
9XB
9XB
9XB
;dB
<jB
=qB
=qB
=qB
=qB
<jB
>wB
=qB
>wB
>wB
=qB
;dB
<jB
>wB
@�B
@�B
A�B
A�B
B�B
A�B
B�B
D�B
D�B
E�B
D�B
D�B
C�B
C�B
C�B
C�B
F�B
G�B
H�B
H�B
H�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
L�B
N�B
M�B
L�B
M�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
S�B
S�B
S�B
S�B
S�B
S�B
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
VB
VB
T�B
VB
W
B
W
B
XB
W
B
W
B
XB
W
B
W
B
XB
YB
YB
XB
YB
YB
XB
YB
ZB
[#B
[#B
[#B
\)B
]/B
]/B
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
`BB
aHB
aHB
aHB
aHB
aHB
`BB
_;B
`BB
`BB
`BB
`BB
bNB
bNB
bNB
cTB
cTB
bNB
aHB
cTB
dZB
cTB
cTB
cTB
cTB
dZB
e`B
e`B
dZB
dZB
e`B
e`B
e`B
e`B
e`B
dZB
e`B
dZB
e`B
ffB
ffB
gmB
gmB
gmB
hsB
hsB
gmB
gmB
gmB
iyB
hsB
gmB
iyB
jB
jB
jB
k�B
k�B
k�B
jB
k�B
k�B
k�B
k�B
k�B
jB
jB
l�B
l�B
l�B
k�B
l�B
l�B
l�B
l�B
n�B
m�B
m�B
m�B
o�B
o�B
n�B
m�B
n�B
n�B
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
r�B
s�B
s�B
s�B
t�B
s�B
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
v�B
v�B
v�B
u�B
v�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BgB`�BQ�BH�BV�BS�BV�BW�B[qBYBT�B^�B_�BZ�BW�BY�BZ�BV�BC-B,B� BJ�BA�BY�BK�B`�B�B�XB��B�%B]dB}B�BsB\B�B~wB|jBw�Bh
BGzBA�B0�BG_B8�B+B)*B+B!HB�B��B��B� B��B�B�fB�9B��B�5B��B�B�$B�FB��Bu�Be�B^�BW�BD3B;�B1�B�BpB
�B
�XB
��B
�B
�/B
�B
��B
�]B
�{B
��B
�OB
�B
��B
�dB
�=G�O�G�O�B
>wB
[#B
`�B
W�B
Q�B
G�B
4�B
%�B
�B	��B	�B	�DB	�B	��B	�hB	�AB	�WB	�vB	��B	�B	�oB	�JB	�B	�'B	��B	�eB	�8B	��B	�~B	��B	�B	�B	��B	�+B	�dB	u?B	f�B	n�B	r�B	n�B	j�B	k6B	nIB	i*B	h�B	e�B	]dB	N"B	T�B	O�B	@�B	>�B	J=B	N"B	N"B	K)B	GEB	CB	>(B	5tB	2-B	-�B	#:B	B	(B	kB	QB	�B	�B	'B	}B	�B	�B	  B�%B�B�]B�B�2B�7B�<B�B��B��B�%B��B��B��B�vB�5B��B�vB��B�!B�B�9B��BzxB{Br�Bg�Bl=By�B{ByrBu�Bj�Bk�Bh�BmCBj�BfLBf2B`BBW
BN<BCaBE9B=<B@BA BMjBOvBQ�BP�BO�BN�BK�BAoB6�BJrBH�BD�B@�B7B9�B8lB?B<6B<B8B49B1AB(�B,�B,B$&B_B�BYB �B-]B)�B'�B*�B/iB,qB(�B#�B�B&fB�B!|B�B%FB#TB�B�BxB�B)BKB�BsB BBVB~BjBVB!-B \BdBsB	RB!-B�B�B�B"hB%FB#nB%FB!�B�B�B �B�B�B�B"�B!�B!�B'�B)yB&�B$�B'�B,�B*�B.�B2|B4nB2�B.�B2�B2�B2�B/�B1�B-�B%,B)B0�B/�B.�B-�B+B2�B:BE�BE�BC�BA�B@BE�BC�B>�B5tB1vB:*B<B=�B>(B?cBD3BGEBL0BLdBM�BMjBNpBUMBS�BN�BX�B^jBX�BZBc�Bf�Bf�Bd&Be,Bn/BoBv�Bq�BshButBz�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�7B�YB��B�yB�UB�iB�UB�oB�}B��B��B��B��B��B��B��B��B��B��B�%B�9B��B�B�0B�B�FB�hB�oB׍BބBߊB�B�B�B��B�'B�B�B�B�B�ZB�]B	[B	uB	uB	�B		�B	�B	�B	�B	�B	�B	�B	"B	#TB	*KB	,"B	./B	./B	/OB	2aB	2GB	3hB	4nB	5�B	5�B	3�B	9�B	:�B	>�B	A�B	D�B	H�B	J	B	I�B	J#B	M6B	PB	SB	T,B	RoB	YKB	\xB	]~B	`�B	c�B	h�B	i�B	j�B	j�B	i�B	i�B	i�B	l�B	q�B	s�B	uB	x�B	y�B	zB	z�B	{B	|B	|B	}B	}�B	}"B	}B	~B	~(B	HB	.B	�AB	�3B	�9B	�mB	�EB	�_B	�YB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	�B	�B	�0B	�6B	�KB	�"B	�CB	�=B	�IB	�[B	�MB	�tB	��B	��B	��B	��B	��B	��B	ðB	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�.B	�:B	�@B	�MB	�SB	�YB	�_B	�KB	�IB	�CB	�CB	�IB	�xB	�IB	�pB	�jB	ޞB	�nB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	�B	�B	�*B	�(B	�.B	�HB
;B
B
 B
 OB
 OB
;B
MB
MB
9B
_B
_B
_B
EB
fB
zB
�B
	�B

rB
~B
jB
PB
dB
dB
�B
�B
pB
}B
}B
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
;B
!�B
$�B
$�B
%B
'B
&B
%B
%B
&B
'B
'B
'B
%,B
'8B
)B
*0B
)*B
+B
*0B
*KB
+B
-)B
-)B
,WB
-CB
-CB
,=B
.IB
./B
/5B
1'B
0UB
0;B
/OB
2aB
2aB
2aB
2aB
3hB
3MB
3hB
3MB
3MB
3MB
3MB
3hB
2GB
/�B
2|B
4�B
5ZB
6zB
5�B
6�B
7�B
7�B
9rB
9�B
:^B
9�B
:xB
9�B
8lB
9rB
9rB
9�B
9�B
;B
<�B
=�B
=�B
=�B
=�B
<�B
>�B
=�B
>�B
>�B
=�B
;�B
<�B
>�B
@�B
@�B
A�B
A�B
B�B
A�B
B�B
D�B
D�B
E�B
D�B
D�B
C�B
C�B
C�B
C�B
F�B
G�B
H�B
H�B
H�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
J�B
K�B
K�B
K�B
K�B
L�B
MB
MB
L�B
L�B
MB
L�B
MB
MB
L�B
N�B
NB
M6B
N"B
QB
R B
R B
SB
R�B
S&B
SB
SB
S&B
S&B
TB
T�B
S�B
S�B
TB
S�B
TB
TB
S&B
TB
T,B
T,B
T,B
U2B
U2B
U2B
U2B
UB
VB
V9B
UB
V9B
W$B
W$B
XB
W$B
W$B
X+B
W$B
W?B
XEB
Y1B
Y1B
X_B
YKB
YKB
X_B
YKB
ZQB
[=B
[=B
[=B
\CB
]/B
]dB
]dB
]dB
]dB
^OB
^jB
^jB
^OB
^OB
_VB
_VB
`\B
abB
aHB
abB
abB
aHB
`\B
_pB
`vB
`\B
`vB
`\B
bNB
b�B
bNB
cTB
cTB
b�B
a|B
cnB
dtB
c�B
cnB
c�B
c�B
d�B
ezB
ezB
dtB
d�B
ezB
ezB
ezB
ezB
ezB
d�B
ezB
d�B
e�B
f�B
f�B
g�B
g�B
g�B
hsB
h�B
g�B
g�B
g�B
i�B
h�B
g�B
i�B
j�B
j�B
j�B
k�B
k�B
k�B
j�B
k�B
k�B
k�B
k�B
k�B
j�B
j�B
l�B
l�B
l�B
k�B
l�B
l�B
l�B
l�B
n�B
m�B
m�B
m�B
o�B
o�B
n�B
m�B
n�B
n�B
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
r�B
s�B
s�B
s�B
t�B
s�B
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
v�B
v�B
v�B
u�B
v�B
v�B
v�B
v�B
w�B
xB
x�B
x�B
x�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<V�b<#�
<#�
<#�
<'�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.12(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201808220034032018082200340320180822003403201808220200152018082202001520180822020015201808230024342018082300243420180823002434  JA  ARFMdecpA19c                                                                20180818093511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180818003513  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180818003517  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180818003518  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180818003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180818003519  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180818003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180818003519  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180818003519  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180818003519  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180818003519  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180818003519                      G�O�G�O�G�O�                JA  ARUP                                                                        20180818005551                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180818154155  CV  JULD            G�O�G�O�F�ӟ                JM  ARCAJMQC2.0                                                                 20180821153403  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180821153403  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180821170015  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180822152434  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                