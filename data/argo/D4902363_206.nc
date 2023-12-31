CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-04T00:35:28Z creation;2018-02-04T00:35:35Z conversion to V3.1;2019-12-19T07:50:12Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180204003528  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_206                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�I��@ڀ1   @�I�www�@:ծ�1���dZȴ9X1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�33A   AffA>ffA`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�3D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D���D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@�\)@�(�Az�A<z�A^{A~{A�
=A�
=A�
=A��
A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$��D%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIq�DI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu��DvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��\D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�?\D���D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A��A��A���A���A�1'A���A��^A�r�A�5?A��PA���A�  A���A�=qA���A�^5A�/A�ĜA�M�A��FA�bNA�"�A���A���A��A���A��A�;dA�A�A���A��yA�ĜA���A��uA�p�A��HA�~�A���A�-A��jA�v�A�VA���A�bNA��7A��A��mA���A�A�~�A�9XA��yA��#A�bNA�"�A��A���A���A�JA��+A�K�A�ȴA���A��/A�`BA�A���A��A���A�{A~-A{�;A{"�AyAx5?Aw&�AwoAv��Au
=Ar �Ao�7An�+Am7LAk�#AiO�AfȴAe��AeXAd��AdAcG�Aa�#A_�wA^�A^I�A]�TA]dZA\�`A[�mAZ�yAY��AX��AW�;AV�AVZAVAU��AUVAS�AQ��AOO�AO�AOoAOAN��ANv�AM�FAK�PAJ��AJE�AI�#AI��AIhsAH�/AH��AHZAHbNAH{AG\)AG�AG"�AF~�ADM�ACp�AC+ABI�AAK�AA
=A@9XA?�A>^5A=�-A<Q�A;dZA;?}A:��A:��A:�A8�DA3��A2z�A1t�A/��A/"�A.jA-�wA-�A,�uA+��A*�!A)O�A(��A(A�A&�+A&5?A&bA%7LA$~�A$I�A#�FA#�A#�A"�DA!�-A!33A ȴA�hA$�AQ�A��A��A��A��Ar�A�AC�AA"�A��A��A��A�FA�uAM�A�TA
=AffA{A��A�A�hAXA?}A
��A
E�A	�;A	hsA	?}A�yA�A5?A�wA�9A�A��A��A�7At�A��A�A�jA��A�^A�A �A ~�@�t�@�^5@�p�@���@�r�@��w@�33@�J@�x�@�?}@���@� �@�ȴ@���@�A�@��y@��@�@��@���@�G�@�\)@�$�@���@���@⟾@���@��y@�p�@ۅ@�
=@ڧ�@��T@�?}@ؼj@��m@�ff@�`B@�9X@�-@Ѻ^@�/@Гu@ͺ^@˕�@�p�@���@��@�E�@���@�p�@�1'@�S�@��T@��j@��P@��H@���@��@�(�@���@�t�@���@�O�@�Q�@�33@��+@�-@�@�p�@���@� �@�"�@��R@�n�@��@��@�(�@��+@�&�@���@��\@�J@��7@��/@�bN@�9X@���@���@���@�+@��@��!@�=q@��h@��w@��H@��\@�5?@�J@��#@���@�p�@�7L@�&�@���@���@��
@��@�{@���@���@�1'@��m@���@�dZ@�v�@���@��9@� �@�(�@� �@�  @��;@���@�|�@�33@�V@�-@�-@���@���@���@�`B@��u@��@�t�@�K�@�E�@�`B@���@��;@�l�@�"�@��!@�@��^@�O�@��@��@��@�1'@���@�^5@�M�@�~�@�\)@��;@��P@�"�@�ȴ@���@�ff@�-@��#@��7@�hs@�/@�Ĝ@���@��@�  @��@��P@�|�@�C�@�
=@��@���@���@�~�@�@�@��T@��^@��7@�`B@�&�@���@��
@�S�@��@��R@�5?@�7L@���@���@��u@��@�bN@�1'@�;@+@~ȴ@~��@~�+@~ff@~V@~5?@}��@|�D@{��@z��@z^5@y�@y��@yx�@yX@y&�@xr�@xb@w�;@w|�@w;d@v�@v�+@up�@u`B@u�@t��@t(�@t9X@s�
@sdZ@r�H@r^5@q�@qx�@qhs@q7L@p��@p1'@o�@ol�@o+@n��@n��@m��@m`B@m?}@m?}@m?}@m�@l�j@l��@l(�@k�m@k��@k�m@kƨ@k�
@k�m@k�F@k@j��@j��@jM�@j�@j=q@jM�@j=q@i��@ix�@i7L@h��@hQ�@hĜ@i%@h��@h��@h�@hr�@hbN@hQ�@hb@f�@e�@e�@dj@cS�@c33@cC�@c��@b��@bJ@a��@`Q�@_��@_;d@^��@^�R@]��@]�h@]p�@]O�@]?}@\�j@[dZ@Z��@Z~�@YX@X�`@XbN@X �@X  @W�w@W�@W\)@V��@Vȴ@VV@U�h@U?}@U/@Tj@SC�@R��@R=q@Q��@Q�7@Qx�@Q�7@Q�@RM�@R~�@R�\@R�\@R�\@Rn�@RJ@Q��@Qx�@QG�@Q7L@Q�@P�`@P�9@P�u@P�u@P�u@Pr�@PQ�@PA�@P  @O�w@O|�@O�@Nȴ@NV@M�@M/@L��@L�@L��@Lz�@LZ@K�m@K�
@Kƨ@K��@KdZ@K"�@J�@J�\@J-@I��@I�#@I��@I��@Ix�@Hr�@Hb@G�;@G�@Gl�@G�@F�y@Fȴ@Fv�@F5?@E�T@E@E��@E`B@E�@D�@D�/@D�j@D�j@D�j@D�j@D�j@D�j@Dz�@D(�@D1@C��@C�
@C��@Ct�@Ct�@CS�@C"�@C@B��@B~�@B^5@B=q@B-@BJ@A�#@A�#@A�7@Ax�@Ahs@Ahs@Ahs@A&�@@��@@�`@@�`@@�9@@A�@?|�@?K�@?+@>��@>�y@>�@>��@>V@=@<��@<��@<��@<j@<�@;S�@:�H@:��@:�!@:�\@:n�@:=q@:J@9�@9��@9�^@9x�@9&�@8��@8�@8A�@8b@7��@7�P@7\)@7\)@7K�@7;d@7�@6ȴ@6��@6��@6��@6�+@65?@6$�@6{@5�T@5��@5�h@5`B@5?}@5?}@5?}@5?}@5/@5�@4�@41@3�F@3�F@3C�@2��@2�!@2n�@2M�@2�@1�^@1x�@1�@0�`@0Q�@/�@/�P@/|�@/+@.�@.��@.ff@-�@-��@-@-�-@-�h@,�@,9X@+�m@+�F@+��@+dZ@+33@+@*��@*~�@*^5@*M�@)�@)�#@)�#@)��@)G�@(��@(�`@(�9@(�@(1'@( �@'�@'��@'�w@'�@'�P@'
=@&��@&E�@&5?@&@%`B@$�@$z�@$Z@$I�@$(�@$1@#�
@#S�@#@"��@"^5@!�#@!��@!�7@!hs@!7L@ ��@ �9@ Q�@ 1'@ b@�@��@�w@��@�@�y@�y@�y@�@�@�R@�+@V@��@`B@�@��@��@z�@j@Z@I�@9X@�@�m@�
@�F@��@dZ@"�@�@n�@-@�@��@hs@G�@&�@�@�@�@�`@Ĝ@bN@A�@1'@1'@ �@  @��@\)@;d@+@��@��@$�@�@��@�@�@`B@/@��@�j@�j@��@z�@Z@(�@��@�
@ƨ@��@��@�@t�@t�@S�@o@��@^5@=q@-@J@��@�#@�^@�^@��@�7@x�@X@7L@�@�`@Ĝ@��@�@�@Q�@b@l�@K�@K�@�@��@V@E�@5?@$�@{@��@�h@p�@/@�@�@�/@�/@�/@�/@�@z�@Z@I�@9X@�
@t�@@
�H@
��@
��@
~�@
~�@
^5@
-@
�@
�@	��@	�#@	��@	��@	hs@	hs@	X@	7L@	&�@	�@	%@��@�`@��@Ĝ@�u@r�@A�@  @�@��@��@��@�P@K�@;d@+@
=@�y@ȴ@��@��@��@ff@V@E�@E�@$�@�@��@��@�-@��@p�@O�@?}111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A��A��A���A���A�1'A���A��^A�r�A�5?A��PA���A�  A���A�=qA���A�^5A�/A�ĜA�M�A��FA�bNA�"�A���A���A��A���A��A�;dA�A�A���A��yA�ĜA���A��uA�p�A��HA�~�A���A�-A��jA�v�A�VA���A�bNA��7A��A��mA���A�A�~�A�9XA��yA��#A�bNA�"�A��A���A���A�JA��+A�K�A�ȴA���A��/A�`BA�A���A��A���A�{A~-A{�;A{"�AyAx5?Aw&�AwoAv��Au
=Ar �Ao�7An�+Am7LAk�#AiO�AfȴAe��AeXAd��AdAcG�Aa�#A_�wA^�A^I�A]�TA]dZA\�`A[�mAZ�yAY��AX��AW�;AV�AVZAVAU��AUVAS�AQ��AOO�AO�AOoAOAN��ANv�AM�FAK�PAJ��AJE�AI�#AI��AIhsAH�/AH��AHZAHbNAH{AG\)AG�AG"�AF~�ADM�ACp�AC+ABI�AAK�AA
=A@9XA?�A>^5A=�-A<Q�A;dZA;?}A:��A:��A:�A8�DA3��A2z�A1t�A/��A/"�A.jA-�wA-�A,�uA+��A*�!A)O�A(��A(A�A&�+A&5?A&bA%7LA$~�A$I�A#�FA#�A#�A"�DA!�-A!33A ȴA�hA$�AQ�A��A��A��A��Ar�A�AC�AA"�A��A��A��A�FA�uAM�A�TA
=AffA{A��A�A�hAXA?}A
��A
E�A	�;A	hsA	?}A�yA�A5?A�wA�9A�A��A��A�7At�A��A�A�jA��A�^A�A �A ~�@�t�@�^5@�p�@���@�r�@��w@�33@�J@�x�@�?}@���@� �@�ȴ@���@�A�@��y@��@�@��@���@�G�@�\)@�$�@���@���@⟾@���@��y@�p�@ۅ@�
=@ڧ�@��T@�?}@ؼj@��m@�ff@�`B@�9X@�-@Ѻ^@�/@Гu@ͺ^@˕�@�p�@���@��@�E�@���@�p�@�1'@�S�@��T@��j@��P@��H@���@��@�(�@���@�t�@���@�O�@�Q�@�33@��+@�-@�@�p�@���@� �@�"�@��R@�n�@��@��@�(�@��+@�&�@���@��\@�J@��7@��/@�bN@�9X@���@���@���@�+@��@��!@�=q@��h@��w@��H@��\@�5?@�J@��#@���@�p�@�7L@�&�@���@���@��
@��@�{@���@���@�1'@��m@���@�dZ@�v�@���@��9@� �@�(�@� �@�  @��;@���@�|�@�33@�V@�-@�-@���@���@���@�`B@��u@��@�t�@�K�@�E�@�`B@���@��;@�l�@�"�@��!@�@��^@�O�@��@��@��@�1'@���@�^5@�M�@�~�@�\)@��;@��P@�"�@�ȴ@���@�ff@�-@��#@��7@�hs@�/@�Ĝ@���@��@�  @��@��P@�|�@�C�@�
=@��@���@���@�~�@�@�@��T@��^@��7@�`B@�&�@���@��
@�S�@��@��R@�5?@�7L@���@���@��u@��@�bN@�1'@�;@+@~ȴ@~��@~�+@~ff@~V@~5?@}��@|�D@{��@z��@z^5@y�@y��@yx�@yX@y&�@xr�@xb@w�;@w|�@w;d@v�@v�+@up�@u`B@u�@t��@t(�@t9X@s�
@sdZ@r�H@r^5@q�@qx�@qhs@q7L@p��@p1'@o�@ol�@o+@n��@n��@m��@m`B@m?}@m?}@m?}@m�@l�j@l��@l(�@k�m@k��@k�m@kƨ@k�
@k�m@k�F@k@j��@j��@jM�@j�@j=q@jM�@j=q@i��@ix�@i7L@h��@hQ�@hĜ@i%@h��@h��@h�@hr�@hbN@hQ�@hb@f�@e�@e�@dj@cS�@c33@cC�@c��@b��@bJ@a��@`Q�@_��@_;d@^��@^�R@]��@]�h@]p�@]O�@]?}@\�j@[dZ@Z��@Z~�@YX@X�`@XbN@X �@X  @W�w@W�@W\)@V��@Vȴ@VV@U�h@U?}@U/@Tj@SC�@R��@R=q@Q��@Q�7@Qx�@Q�7@Q�@RM�@R~�@R�\@R�\@R�\@Rn�@RJ@Q��@Qx�@QG�@Q7L@Q�@P�`@P�9@P�u@P�u@P�u@Pr�@PQ�@PA�@P  @O�w@O|�@O�@Nȴ@NV@M�@M/@L��@L�@L��@Lz�@LZ@K�m@K�
@Kƨ@K��@KdZ@K"�@J�@J�\@J-@I��@I�#@I��@I��@Ix�@Hr�@Hb@G�;@G�@Gl�@G�@F�y@Fȴ@Fv�@F5?@E�T@E@E��@E`B@E�@D�@D�/@D�j@D�j@D�j@D�j@D�j@D�j@Dz�@D(�@D1@C��@C�
@C��@Ct�@Ct�@CS�@C"�@C@B��@B~�@B^5@B=q@B-@BJ@A�#@A�#@A�7@Ax�@Ahs@Ahs@Ahs@A&�@@��@@�`@@�`@@�9@@A�@?|�@?K�@?+@>��@>�y@>�@>��@>V@=@<��@<��@<��@<j@<�@;S�@:�H@:��@:�!@:�\@:n�@:=q@:J@9�@9��@9�^@9x�@9&�@8��@8�@8A�@8b@7��@7�P@7\)@7\)@7K�@7;d@7�@6ȴ@6��@6��@6��@6�+@65?@6$�@6{@5�T@5��@5�h@5`B@5?}@5?}@5?}@5?}@5/@5�@4�@41@3�F@3�F@3C�@2��@2�!@2n�@2M�@2�@1�^@1x�@1�@0�`@0Q�@/�@/�P@/|�@/+@.�@.��@.ff@-�@-��@-@-�-@-�h@,�@,9X@+�m@+�F@+��@+dZ@+33@+@*��@*~�@*^5@*M�@)�@)�#@)�#@)��@)G�@(��@(�`@(�9@(�@(1'@( �@'�@'��@'�w@'�@'�P@'
=@&��@&E�@&5?@&@%`B@$�@$z�@$Z@$I�@$(�@$1@#�
@#S�@#@"��@"^5@!�#@!��@!�7@!hs@!7L@ ��@ �9@ Q�@ 1'@ b@�@��@�w@��@�@�y@�y@�y@�@�@�R@�+@V@��@`B@�@��@��@z�@j@Z@I�@9X@�@�m@�
@�F@��@dZ@"�@�@n�@-@�@��@hs@G�@&�@�@�@�@�`@Ĝ@bN@A�@1'@1'@ �@  @��@\)@;d@+@��@��@$�@�@��@�@�@`B@/@��@�j@�j@��@z�@Z@(�@��@�
@ƨ@��@��@�@t�@t�@S�@o@��@^5@=q@-@J@��@�#@�^@�^@��@�7@x�@X@7L@�@�`@Ĝ@��@�@�@Q�@b@l�@K�@K�@�@��@V@E�@5?@$�@{@��@�h@p�@/@�@�@�/@�/@�/@�/@�@z�@Z@I�@9X@�
@t�@@
�H@
��@
��@
~�@
~�@
^5@
-@
�@
�@	��@	�#@	��@	��@	hs@	hs@	X@	7L@	&�@	�@	%@��@�`@��@Ĝ@�u@r�@A�@  @�@��@��@��@�P@K�@;d@+@
=@�y@ȴ@��@��@��@ff@V@E�@E�@$�@�@��@��@�-@��@p�@O�@?}111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
=B
=B
=B
=B
=B
=B
=BDBDB
=BDB
=B
=B
=B
=B
=B
=B
=B
=B	7B1BBB��B�B��B��B�B�ZB�
B�mB�`B�`B�B��B��B��BĜB�qB�LB�LB�9B�B��B��B��B��B��B��B��B��B��B�\B�VB�%Bp�BH�B�B(�B�B�B��B��B��B�B{�BO�BL�B=qB
=BDB
�B
�B
�B
��B
�B
�mB
��B
��B
��B
��B
B
�B
��B
��B
��B
��B
�PB
�B
w�B
ffB
S�B
`BB
S�B
K�B
F�B
N�B
D�B
)�B
PB	��B
B	��B	�B	��B	��B	��B	��B	��B	��B	�RB	��B	��B	��B	��B	��B	��B	��B	�=B	�+B	y�B	|�B	w�B	u�B	y�B	w�B	p�B	ffB	Q�B	@�B	>wB	YB	]/B	[#B	XB	M�B	@�B	)�B	49B	>wB	<jB	>wB	<jB	6FB	:^B	6FB	8RB	33B	,B	.B	,B	�B	
=B	VB	�B	DB	B	
=B	  B��B��B�B�B�mB�B�B�TB��B�wB��B�B�XB�B�RB�FB�'B�-B�!B��B��B��B��B��B�\B��B��B��B�oB��B�uB��B�oB�JB�+B�B�Bu�BffBaHBaHB^5B`BB[#BXB\)BXBK�BN�BG�BH�BL�BQ�BG�BN�BM�BG�BG�BN�BO�BP�BO�BL�BL�BF�BE�BG�BE�BI�BF�BE�B>wB<jB7LB9XBA�BA�BA�B>wB5?B-B%�B1'B8RB8RB1'B1'B/B.B33B5?B6FB49B49B0!B49B6FB33B/B'�B&�B�B�BB�B$�B"�B"�B�B�B�B�B�B�B�B�B�B$�B&�B#�B#�B"�B�B�B�B�B�B!�B �B�BPBoB�B�B(�B)�B-B+B%�B$�B!�B#�B&�B+B.B%�B,B33B0!B,B+B/B2-B7LB;dB;dB<jB9XB7LB7LB=qB=qB;dB6FB6FB49B5?B>wBC�BL�BN�BO�BR�BW
BW
BXBW
BVBXBW
BS�BR�BN�B^5BhsBiyBl�Bl�Bm�Bn�Bn�Bp�Bn�Bm�BjBm�Bo�Bs�B~�B�B�B�B�B}�B|�B�JB�PB�hB�{B��B��B��B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�?B�XB�dB�wBBŢBȴB��B��B��BȴB�/B�NB�fB�B��B�B��B��B��B��B	  B	B	B	%B	%B	%B		7B	DB	1B	hB	�B	�B	�B	�B	�B	 �B	!�B	"�B	!�B	(�B	'�B	'�B	'�B	(�B	'�B	%�B	#�B	'�B	+B	+B	,B	-B	6FB	<jB	?}B	?}B	>wB	>wB	>wB	?}B	D�B	F�B	G�B	H�B	H�B	G�B	F�B	F�B	M�B	R�B	XB	YB	[#B	]/B	^5B	^5B	]/B	aHB	dZB	dZB	gmB	iyB	iyB	hsB	o�B	o�B	o�B	p�B	t�B	v�B	w�B	v�B	y�B	|�B	�B	�B	�B	�B	�B	�%B	�7B	�=B	�DB	�bB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�!B	�-B	�-B	�'B	�'B	�3B	�3B	�3B	�3B	�'B	�3B	�RB	�FB	�RB	B	ĜB	ƨB	ÖB	ÖB	ĜB	B	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�/B	�BB	�HB	�HB	�NB	�HB	�HB	�NB	�NB	�TB	�mB	�B	�yB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
%B
+B
+B
%B
+B
1B
1B
1B

=B

=B

=B

=B
	7B
+B
1B

=B
DB
JB
PB
VB
\B
PB
PB
VB
VB
\B
bB
hB
oB
oB
uB
{B
{B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
!�B
!�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
$�B
$�B
#�B
%�B
'�B
(�B
%�B
&�B
'�B
'�B
&�B
'�B
(�B
(�B
)�B
+B
+B
+B
)�B
)�B
'�B
(�B
,B
-B
,B
+B
-B
-B
.B
.B
.B
/B
/B
0!B
0!B
1'B
2-B
33B
33B
33B
33B
49B
49B
5?B
6FB
6FB
5?B
33B
49B
7LB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
<jB
;dB
<jB
=qB
<jB
:^B
<jB
=qB
=qB
<jB
=qB
>wB
>wB
>wB
?}B
>wB
>wB
<jB
=qB
?}B
@�B
?}B
=qB
?}B
@�B
B�B
C�B
C�B
B�B
A�B
@�B
B�B
C�B
B�B
C�B
E�B
F�B
F�B
E�B
F�B
F�B
E�B
G�B
G�B
G�B
H�B
H�B
G�B
G�B
H�B
J�B
J�B
J�B
I�B
I�B
H�B
H�B
H�B
I�B
J�B
K�B
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
M�B
M�B
M�B
M�B
L�B
N�B
N�B
O�B
O�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
P�B
R�B
R�B
R�B
R�B
Q�B
Q�B
Q�B
S�B
S�B
R�B
R�B
Q�B
S�B
T�B
VB
VB
VB
T�B
T�B
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
XB
YB
XB
XB
XB
W
B
W
B
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
ZB
ZB
ZB
[#B
[#B
\)B
\)B
[#B
ZB
YB
\)B
]/B
\)B
[#B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
aHB
`BB
aHB
`BB
_;B
`BB
`BB
`BB
`BB
^5B
_;B
`BB
bNB
cTB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
cTB
cTB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
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
iyB
jB
jB
jB
jB
jB
jB
l�B
l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
=B
XB
XB
=B
=B
=B
XBDBDB
XBDB
XB
=B
=B
=B
XB
XB
XB
=B	RB1B9BoB��B�B�PB�tB�iB�B��B�B�LB�2B�B��B�uB��BżB��B�B��B��B�/B�B�LB�TB�VB�#B��B��B��B�B��B��B��G�O�G�O�B#�B*�B�G�O�B��B�,B�G�O�B� BT�BO�B@ B\BVB
�B
�B
�CB
�zB
�GB
�B
�SB
�VB
��B
̘B
��B
�UB
��B
��B
��B
��B
��B
��B
y�B
h�B
V�B
a�B
U�B
M�B
G�B
OvB
F%G�O�B
�B
 �B
�B	��B	�B	�&B	�pB	�B	ӏB	ΥB	��B	��B	��B	�B	��B	�sB	��B	��B	�YB	��B	��B	{�B	~BB	x�B	v�B	z�B	xlB	q[B	g�B	T,B	C-B	AB	YKB	]IB	[WB	X_B	N�B	A�B	,WB	5?B	>�B	=B	>�B	<�B	6�B	:�B	6�B	8�B	3�B	,�B	.�B	,qB	!-B	�B	vB	$B	~B	YB	
�B	;B�2B��B��B�=B�B�B�]B�B�G�O�G�O�B�;B��B�IB�>B�fB�GB�3B�'B�XB�LB�jB��B��B�hB�5B�'B��B�uB��B�aB��B�@B�6B�fB�B�Bw�Bh�Bc�Bc:B`Ba�B\�BY�B]BYeBM�BPBI�BJ=BM�BRoBIBOvBN�BH�BH�BOBBPHBQ4BPHBMPBMPBGzBFYBHfBFtBJ=BG+BFB?}B=qB8�B:*BA�BA�BA�B>�B6FB.�B'�B2-B8�B8�B2-B2B0!B/ B3�B5�B6�B4�B4�B0�B4�B6�B3�B/�B)B'�B!HB�G�O�B�B%`B#�B#�B�B�B�B�B�B�B�B�B�B%`B'mB$tB$ZB#TB �B�B~B�B�B"NB!|B�B\B�B�B�B)�B*�B-�B+�B&�B%�B"�B$�B'�B+�B.�B'B,�B3hB0�B,�B,"B0B2�B7�B;�B;�B<�B9�B8B8B=�B=�B<B7B7B5tB6zB?�BD�BM6BOvBPbBS[BWYBWYBX_BWYBVmBX_BWsBT{BS�BPHB^�Bh�Bi�Bl�Bl�Bm�Bn�Bn�Bp�Bo Bm�BkQBnIBp�Bt�BHB��B�aB�aB�aB~�B}�B�dB��B��B��B��B��B��B��B��B�,B��B��B��B��B�B�B�+B�KB�B�@B��B��B��B��B��B��B��B�B��B��B�B��B� B͟BɺB�dB�hB�LB�IB��B�B�B�$B�.B�HB	 4B	oB	{B	YB	YB	tB		lB	xB	�B	�B	�B	�B	�B	�B	�B	!B	!�B	# B	"NB	)B	(>B	($B	(>B	)DB	(XB	&fB	$�B	(sB	+QB	+�B	,�B	-�B	6�B	<�B	?�B	?�B	>�B	>�B	>�B	?�B	D�B	F�B	G�B	H�B	H�B	G�B	GB	G+B	N<B	S[B	X+B	YeB	[WB	]dB	^OB	^OB	]~B	a�B	d�B	d�B	g�B	i�B	i�B	h�B	o�B	o�B	o�B	p�B	t�B	wB	xB	w2B	zB	}<B	�;B	�MB	�3B	�uB	�gB	�tB	�RB	�rB	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	�B	�4B	��B	�*B	�*B	�6B	�"B	�"B	�6B	�0B	�DB	�B	�KB	�QB	�B	�-B	�aB	�AB	�[B	�3B	�MB	�hB	��G�O�B	��B	��G�O�B	��B	B	ĜB	��G�O�B	��B	��B	�B	�B	��B	��B	�B	�)B	��B	�&B	�&B	�@G�O�B	�vB	�aB	�EG�O�B	�xB	�dB	�vB	�bB	�bB	�B	�|B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��G�O�B	��B	��B	��B	��B	��B	��B	�B	��B	�	B	�	B	�	B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�*B	�B	�<B	�<B	�VB
 B
GB
MB
MB
3B
MB
?B
_B
_B
?B
_B
fB
�B
fB

rB

rB

rB

XB
	lG�O�B
�B

XB
xB
~B
�B
�B
�G�O�B
�B
pB
�B
vB
}B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
 �B
#B
#�B
#B
#B
#B
#B
#B
#B
"�B
#B
"B
"B
#�B
#�B
$B
$B
$�B
%B
%�B
%�B
$�B
%B
$B
%�B
(
B
)G�O�B
'B
'�B
(
B
'B
(
B
)*B
)B
*0B
+B
+B
+B
*0B
*0G�O�B
)*B
,=B
-)B
,WB
+QB
-)B
-CB
.IB
./B
.IB
/5B
/iB
0UB
0oB
1vB
2aB
3hB
3MB
3hB
3MB
4nB
4�B
5ZB
6FB
6zB
5tG�O�B
4�B
7fB
8lB
9�B
9rB
9rB
9rB
9�B
:�B
;�B
<�B
;�B
<�B
=�B
<�G�O�B
<�B
=�B
=�B
<�B
=�B
>�B
>�B
>�B
?}B
>�B
>�G�O�B
=�B
?�B
@�B
?�G�O�B
?�B
@�B
B�B
C�B
C�B
B�B
A�B
@�B
B�B
C�B
B�B
C�B
E�B
F�B
F�B
E�B
F�B
F�B
E�B
G�B
G�B
G�B
H�B
H�B
G�B
G�B
H�B
J�B
J�B
J�B
I�B
I�B
H�B
H�B
IB
I�B
J�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
NB
NB
M�B
M�B
NB
M�B
NB
NB
MB
OB
OB
O�B
PB
RB
RB
RB
Q�B
RB
R B
QB
Q B
SB
SB
SB
SB
RB
R B
R B
S�B
TB
SB
SB
R:B
T,B
U2B
V9B
VB
VB
U2B
U2B
W
B
W$B
W?B
W$B
W$B
W?B
W?B
X+B
X+B
X+B
X+B
Y1B
X+B
XB
X+B
W?B
W?B
XEB
YKB
Z7B
Z7B
Z7B
Z7B
Z7B
Z7B
Z7B
Z7B
Z7B
Z7B
Z7B
ZQB
ZQB
[WB
[=B
\)B
\CB
[WB
ZQB
YeB
\)B
]/B
\CB
[qB
]dB
^5B
^OB
^OB
^OB
^jB
^jB
_VB
_VB
_VB
`\B
aHB
`BB
abB
`\B
_VB
`vB
`vB
`\B
`vG�O�B
_�B
`�B
bhB
c�B
b�B
cTB
cnB
cnB
c�B
dtB
dZB
cnB
c�B
d�B
d�B
ezB
e`B
e`B
e�B
ezB
f�B
f�B
ffB
f�B
ffB
ezB
e�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
g�B
h�B
h�B
iyB
i�B
iyB
i�B
iyB
i�B
i�B
i�B
i�B
j�B
jB
j�B
j�B
j�B
j�B
l�B
l�111111111111111111111111111111111111111111111111111111144111411141111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411411114111111111111411141111111111111111111111114111111111111111111111111111111111111111111114111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111411111111111111111111111111411111111111111141111111111141111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�<#�
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
G�O�<#�
<#�
G�O�<#�
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
G�O�<#�
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
G�O�<#�
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
G�O�<#�
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
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.12(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802080041162018020800411620180208004116201806221237142018062212371420180622123714201804050433562018040504335620180405043356  JA  ARFMdecpA19c                                                                20180204093521  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180204003528  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180204003530  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180204003531  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180204003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180204003532  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180204003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180204003532  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180204003535  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180204003535                      G�O�G�O�G�O�                JA  ARUP                                                                        20180204005612                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180204153038  CV  JULD            G�O�G�O�F�M�                JM  ARSQJMQC2.0                                                                 20180205000000  CF  PSAL_ADJUSTED_QCB�  D�@ G�O�                JM  ARCAJMQC2.0                                                                 20180207154116  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180207154116  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193356  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033714  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                