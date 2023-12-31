CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-12T00:35:28Z creation;2018-03-12T00:35:32Z conversion to V3.1;2019-12-19T07:47:21Z update;     
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ݔ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180312003528  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_218                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�R��3��1   @�R�`��@:*q�i�C�df9XbN1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @y��@�  A   A   A@  A`  A���A�  A�  A�  A�  A�ffA�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ Dü�D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�C3Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@q�@�(�@�(�A{A>{A^{A�A�
=A�
=A�
=A�
=A�p�A�
=A�=qA�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa��Cc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJ~�DJ��DKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�8�D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��\D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dø�D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�\Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�?\D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D���D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�?\D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��\D�E�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�r�A�n�A�v�A�r�A�r�A�v�A�v�A�v�A�t�A�t�A�x�A�x�A�x�A�z�A�~�A�|�A�z�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�x�A�x�A��A�z�A�p�A�ffA�ffA�VA�Q�A�A���A�ĜA�?}A�p�A���A��A�VA���A�n�A�-A�Q�A���A��A��uA���A�9XA��RA�A�A�9XA�A��RA���A�jA�G�A�x�A�9XA��A���A�l�A�  A�M�A��A�^5A�oA��A���A���A�5?A��A��A�ZA���A�7LA�E�AC�A|�AyAx�\Aw�PAu��As��Aq��Aq7LAp�9Ap1'Ao��An��Am�-AlVAkO�Aj�RAi�Ag"�Af^5Ae�;Ae�Adv�AcO�AbĜAa�A`�9A_�A^��A^ffA\��A\M�A["�AY��AYXAX�9AX1AWXAUl�AT�RAT  AS|�AR��AR{AQl�AP��AP �AO��AO%AM�-AK��AJ��AHZAGAF��AF�uAF�AE/AC�#AB1A@�A?��A?XA>�uA=�A=�hA=l�A=%A;�A;|�A:�9A:v�A:=qA9��A8^5A7�A6��A6�A6$�A5��A4�A4E�A3p�A2��A2�uA2M�A1�;A0jA/�-A/�A.�`A.�A-��A-C�A,��A,�DA+�wA+�A*�A*�uA)�A)
=A(�A'\)A&v�A%�A%&�A$bNA$$�A#ƨA#33A"�A"��A"Q�A!A!"�A �uA��A�AO�A��A�AQ�A�AVAƨA��AO�Az�A�AC�A��AVA��Al�A  A�A�mA
�HA
bA	
=An�A�#A33AA�A�A��A{A�#A�^A�A��A�hA�7Al�A�A�hA"�A �9A -@�+@�v�@��T@��7@���@�n�@�&�@��9@�9X@�;d@�v�@�V@�!@�hs@��/@@��y@�G�@�u@�;d@�"�@��T@��@�@���@��@��
@�=q@��@���@�-@�dZ@�X@�(�@�;d@��@�-@���@��#@�;d@˥�@�o@�ȴ@�v�@�-@ɡ�@�dZ@�Ĝ@�M�@�?}@�A�@��@�;d@�M�@��@�I�@��m@�C�@�@��#@��D@�1'@��F@��P@�|�@��@�~�@�v�@���@��`@���@�1@��P@�5?@�O�@���@�Q�@��@��!@��T@�X@���@�j@�ƨ@�o@�=q@�p�@���@�\)@�n�@���@�p�@�/@���@�Z@�  @��
@��@�dZ@�o@�
=@�ȴ@�~�@�=q@�$�@���@�1'@�b@��m@��@��H@��@�x�@��j@�z�@�A�@��;@��F@��@�ȴ@�v�@�5?@�J@�@�@�`B@���@���@��@�A�@��
@��@���@�C�@���@���@�I�@��;@�l�@�K�@�;d@�"�@��@�~�@��@�?}@��j@�A�@��;@���@�+@��H@�^5@�J@���@�p�@��@���@�I�@� �@�ƨ@��@�\)@�;d@��y@���@���@��R@�^5@���@�O�@��`@��@��;@��F@���@��@�|�@�"�@��R@��\@�l�@�C�@�S�@��R@���@��h@��7@�?}@���@��9@���@��@���@��j@�bN@��w@��!@�~�@�E�@�J@���@�O�@�hs@��h@�O�@��@�bN@�1'@�@|�@;d@~�@~��@|�@{��@{@z��@z��@z�\@zM�@zJ@y��@y�7@y&�@x�`@xbN@x  @w�P@w�@v��@vV@u�@uV@uV@t�D@s�F@r��@rM�@q�@q��@q��@p��@pĜ@pQ�@p �@o�@o��@o�P@n��@n�R@n�R@n��@n�+@nff@nV@n$�@m�@m�@m/@m�@mV@l��@l�j@lj@l(�@k�m@k�m@k��@kS�@kC�@k"�@j�H@j�H@j��@j�!@j��@j~�@jn�@j=q@iX@h�`@h�9@h1'@g\)@f�@f�R@f�R@f��@fv�@fV@f{@f@e�T@e@e��@e�@dZ@d(�@d�@c�m@co@a��@a�7@ax�@aX@aX@a7L@`��@`Q�@_|�@_;d@^�y@^�R@^��@^{@]�-@]�-@]�-@]�-@]�@\�j@\j@[�
@[�F@[��@[t�@["�@Z�!@Z~�@Z=q@Y�@Y�^@Yhs@X��@XbN@X  @W�@W�;@W�w@W\)@W
=@V�y@V�+@VE�@V@U��@U��@U`B@TZ@R�H@R�H@R��@R��@R�!@Q��@Q�@P��@Pr�@Ol�@N��@M�T@M`B@L��@L�j@L�@Lj@L1@Kƨ@KC�@J��@Jn�@J-@J�@I��@I�#@I��@Ihs@I7L@I7L@I&�@I&�@I%@I%@H�`@H��@Hr�@Hb@F�@E��@Ep�@E?}@EV@D�/@D�j@Dz�@C��@C�F@Ct�@C"�@B��@B^5@BJ@A��@Ax�@AG�@A&�@A�@@��@@�@?�;@?�w@?�@?�P@?|�@?|�@?|�@?l�@?l�@?l�@?K�@?K�@?;d@?�@?�@>��@>{@>@>@=@=�h@=�@=p�@=?}@=V@<��@;ƨ@;t�@;S�@;S�@;C�@;33@;"�@;o@:��@:n�@:-@9G�@8��@8�@7�;@7��@7�;@7��@7��@7|�@7\)@7;d@6��@6v�@6V@6{@5�@5@5@5�@4�@4z�@4j@49X@41@3�F@3t�@3dZ@3o@2�\@2~�@2=q@1��@1��@1X@0�`@0Ĝ@0Ĝ@0��@0r�@0Q�@0b@/�w@/��@/|�@/K�@/�@/+@/+@/�@/�@/
=@.�y@.V@.{@-�T@-@-�h@-`B@-V@,�@,j@+�
@+��@+C�@*��@*��@*~�@*-@*J@)��@)�@)��@)x�@)G�@)&�@)%@(�`@(��@(Ĝ@(��@(Q�@(b@'�@'�w@'�P@'|�@'K�@'�@&��@&ȴ@&��@&@%��@%@%�@%?}@$��@$�/@$�j@$�@$j@$(�@$1@#ƨ@#�
@#ƨ@#��@#t�@#o@"�\@!�@!�@!��@!X@!7L@ �`@ Ĝ@ ��@ A�@�@+@
=@ȴ@��@�+@v�@ff@V@5?@$�@{@�@��@�h@?}@/@�@��@�@�@��@z�@I�@1@ƨ@�F@��@��@�@dZ@C�@o@�@��@=q@�@�#@��@��@��@�@Ĝ@r�@1'@  @�@|�@
=@�R@��@E�@$�@�T@�-@��@?}@��@j@�@��@��@��@�m@�m@ƨ@��@��@C�@"�@o@o@@�!@^5@-@�@�^@�^@��@hs@�@��@�`@�`@Ĝ@��@�u@�u@�@bN@1'@�@��@�w@�w@��@|�@�@�@�+@ff@E�@5?@{@{@{@�@�@�T@��@�-@p�@`B@/@V@�@��@�@j@I�@(�@�m@�F@t�@S�@33@"�@o@
�H@
�\@
n�@
~�@
~�@
=q@
J@	�#@	��@	�7@	X@	G�@	7L@	&�@	&�@	�@	�@	%@	%@��@�`@�9@�9@�u@�@A�@�@�@��@K�@�@��@�+@ff@5?@$�@�@�h@?}@�@V@V@V@�@��@�j@z�@I�@9X@�@��@ƨ@dZ@S�@33@33@"�@@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�r�A�n�A�v�A�r�A�r�A�v�A�v�A�v�A�t�A�t�A�x�A�x�A�x�A�z�A�~�A�|�A�z�A�|�A�|�A�|�A�|�A�|�A�|�A�|�A�~�A�|�A�|�A�|�A�x�A�x�A��A�z�A�p�A�ffA�ffA�VA�Q�A�A���A�ĜA�?}A�p�A���A��A�VA���A�n�A�-A�Q�A���A��A��uA���A�9XA��RA�A�A�9XA�A��RA���A�jA�G�A�x�A�9XA��A���A�l�A�  A�M�A��A�^5A�oA��A���A���A�5?A��A��A�ZA���A�7LA�E�AC�A|�AyAx�\Aw�PAu��As��Aq��Aq7LAp�9Ap1'Ao��An��Am�-AlVAkO�Aj�RAi�Ag"�Af^5Ae�;Ae�Adv�AcO�AbĜAa�A`�9A_�A^��A^ffA\��A\M�A["�AY��AYXAX�9AX1AWXAUl�AT�RAT  AS|�AR��AR{AQl�AP��AP �AO��AO%AM�-AK��AJ��AHZAGAF��AF�uAF�AE/AC�#AB1A@�A?��A?XA>�uA=�A=�hA=l�A=%A;�A;|�A:�9A:v�A:=qA9��A8^5A7�A6��A6�A6$�A5��A4�A4E�A3p�A2��A2�uA2M�A1�;A0jA/�-A/�A.�`A.�A-��A-C�A,��A,�DA+�wA+�A*�A*�uA)�A)
=A(�A'\)A&v�A%�A%&�A$bNA$$�A#ƨA#33A"�A"��A"Q�A!A!"�A �uA��A�AO�A��A�AQ�A�AVAƨA��AO�Az�A�AC�A��AVA��Al�A  A�A�mA
�HA
bA	
=An�A�#A33AA�A�A��A{A�#A�^A�A��A�hA�7Al�A�A�hA"�A �9A -@�+@�v�@��T@��7@���@�n�@�&�@��9@�9X@�;d@�v�@�V@�!@�hs@��/@@��y@�G�@�u@�;d@�"�@��T@��@�@���@��@��
@�=q@��@���@�-@�dZ@�X@�(�@�;d@��@�-@���@��#@�;d@˥�@�o@�ȴ@�v�@�-@ɡ�@�dZ@�Ĝ@�M�@�?}@�A�@��@�;d@�M�@��@�I�@��m@�C�@�@��#@��D@�1'@��F@��P@�|�@��@�~�@�v�@���@��`@���@�1@��P@�5?@�O�@���@�Q�@��@��!@��T@�X@���@�j@�ƨ@�o@�=q@�p�@���@�\)@�n�@���@�p�@�/@���@�Z@�  @��
@��@�dZ@�o@�
=@�ȴ@�~�@�=q@�$�@���@�1'@�b@��m@��@��H@��@�x�@��j@�z�@�A�@��;@��F@��@�ȴ@�v�@�5?@�J@�@�@�`B@���@���@��@�A�@��
@��@���@�C�@���@���@�I�@��;@�l�@�K�@�;d@�"�@��@�~�@��@�?}@��j@�A�@��;@���@�+@��H@�^5@�J@���@�p�@��@���@�I�@� �@�ƨ@��@�\)@�;d@��y@���@���@��R@�^5@���@�O�@��`@��@��;@��F@���@��@�|�@�"�@��R@��\@�l�@�C�@�S�@��R@���@��h@��7@�?}@���@��9@���@��@���@��j@�bN@��w@��!@�~�@�E�@�J@���@�O�@�hs@��h@�O�@��@�bN@�1'@�@|�@;d@~�@~��@|�@{��@{@z��@z��@z�\@zM�@zJ@y��@y�7@y&�@x�`@xbN@x  @w�P@w�@v��@vV@u�@uV@uV@t�D@s�F@r��@rM�@q�@q��@q��@p��@pĜ@pQ�@p �@o�@o��@o�P@n��@n�R@n�R@n��@n�+@nff@nV@n$�@m�@m�@m/@m�@mV@l��@l�j@lj@l(�@k�m@k�m@k��@kS�@kC�@k"�@j�H@j�H@j��@j�!@j��@j~�@jn�@j=q@iX@h�`@h�9@h1'@g\)@f�@f�R@f�R@f��@fv�@fV@f{@f@e�T@e@e��@e�@dZ@d(�@d�@c�m@co@a��@a�7@ax�@aX@aX@a7L@`��@`Q�@_|�@_;d@^�y@^�R@^��@^{@]�-@]�-@]�-@]�-@]�@\�j@\j@[�
@[�F@[��@[t�@["�@Z�!@Z~�@Z=q@Y�@Y�^@Yhs@X��@XbN@X  @W�@W�;@W�w@W\)@W
=@V�y@V�+@VE�@V@U��@U��@U`B@TZ@R�H@R�H@R��@R��@R�!@Q��@Q�@P��@Pr�@Ol�@N��@M�T@M`B@L��@L�j@L�@Lj@L1@Kƨ@KC�@J��@Jn�@J-@J�@I��@I�#@I��@Ihs@I7L@I7L@I&�@I&�@I%@I%@H�`@H��@Hr�@Hb@F�@E��@Ep�@E?}@EV@D�/@D�j@Dz�@C��@C�F@Ct�@C"�@B��@B^5@BJ@A��@Ax�@AG�@A&�@A�@@��@@�@?�;@?�w@?�@?�P@?|�@?|�@?|�@?l�@?l�@?l�@?K�@?K�@?;d@?�@?�@>��@>{@>@>@=@=�h@=�@=p�@=?}@=V@<��@;ƨ@;t�@;S�@;S�@;C�@;33@;"�@;o@:��@:n�@:-@9G�@8��@8�@7�;@7��@7�;@7��@7��@7|�@7\)@7;d@6��@6v�@6V@6{@5�@5@5@5�@4�@4z�@4j@49X@41@3�F@3t�@3dZ@3o@2�\@2~�@2=q@1��@1��@1X@0�`@0Ĝ@0Ĝ@0��@0r�@0Q�@0b@/�w@/��@/|�@/K�@/�@/+@/+@/�@/�@/
=@.�y@.V@.{@-�T@-@-�h@-`B@-V@,�@,j@+�
@+��@+C�@*��@*��@*~�@*-@*J@)��@)�@)��@)x�@)G�@)&�@)%@(�`@(��@(Ĝ@(��@(Q�@(b@'�@'�w@'�P@'|�@'K�@'�@&��@&ȴ@&��@&@%��@%@%�@%?}@$��@$�/@$�j@$�@$j@$(�@$1@#ƨ@#�
@#ƨ@#��@#t�@#o@"�\@!�@!�@!��@!X@!7L@ �`@ Ĝ@ ��@ A�@�@+@
=@ȴ@��@�+@v�@ff@V@5?@$�@{@�@��@�h@?}@/@�@��@�@�@��@z�@I�@1@ƨ@�F@��@��@�@dZ@C�@o@�@��@=q@�@�#@��@��@��@�@Ĝ@r�@1'@  @�@|�@
=@�R@��@E�@$�@�T@�-@��@?}@��@j@�@��@��@��@�m@�m@ƨ@��@��@C�@"�@o@o@@�!@^5@-@�@�^@�^@��@hs@�@��@�`@�`@Ĝ@��@�u@�u@�@bN@1'@�@��@�w@�w@��@|�@�@�@�+@ff@E�@5?@{@{@{@�@�@�T@��@�-@p�@`B@/@V@�@��@�@j@I�@(�@�m@�F@t�@S�@33@"�@o@
�H@
�\@
n�@
~�@
~�@
=q@
J@	�#@	��@	�7@	X@	G�@	7L@	&�@	&�@	�@	�@	%@	%@��@�`@�9@�9@�u@�@A�@�@�@��@K�@�@��@�+@ff@5?@$�@�@�h@?}@�@V@V@V@�@��@�j@z�@I�@9X@�@��@ƨ@dZ@S�@33@33@"�@@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BƨBB�LB��By�Bu�Bl�Bp�BbNBL�B'�BB�B  B��B�ZB�qB��BjB�1B�{B�+B|�B[#B'�B#�B'�B �B1B
�HBB  B
��B
�B
�fB
�B
��B
��B
��B
ɺB
�wB
�B
��B
�B
��B
�uB
�B
}�B
cTB
<jB
33B
!�B
0!B
+B
�B
1B	��B
	7B

=B
B	��B	�B	�`B	�
B	��B	��B	�wB	�B	�-B	�-B	�B	��B	��B	��B	�uB	�+B	�B	�+B	� B	n�B	p�B	gmB	bNB	ffB	aHB	ZB	S�B	A�B	M�B	N�B	M�B	J�B	G�B	H�B	C�B	B�B	?}B	7LB	'�B	�B	�B	%B	DB	�B	�B	\B	B��B�B�yB��B�B�B�B��B��B�B�ZB�B�ZB�B�fB�BB��B��B��B�/B�
B��B��B��BǮBɺBɺBȴB��B�9B�FB�wB�LB�B�?B�?B�'B�-B��B��B�B��B��B��B�hB�hB�JB�JB�hB�DB�hB�VB�1B�JB�7B�%B� Bw�Bw�B`BBr�Bs�Bu�Bw�Bo�Be`BXB_;BM�B[#BXBVBO�B9XBM�BD�B<jB>wB:^B=qB;dB9XB7LB:^B7LB49B+B+B33B33B:^B<jB=qB=qB;dB9XB33B)�B�B(�B)�B(�B'�B,B+B+B#�B�B�B&�B#�B�B�B{BhB{B�B1B��BJB�BVB��B�B#�B+B&�B �B�B{B�BVB	7BBPB�B�B!�B�B �B�B+B��B"�B%�B#�B�B{B%B��B
=B�B�B%�B&�B"�B�B1'B1'B-B/B(�B(�B49B7LB;dB;dB8RB8RB;dB6FB5?B=qB7LB8RB33B:^B?}B?}B>wB>wBC�BE�BH�BI�BI�BK�BN�BN�BO�BM�BR�B[#B[#B`BB_;B_;BbNBe`BffBffBffBk�BjBk�Bl�Bl�BffBjBu�Bs�Bq�Bp�Bo�Bw�By�B~�B�B�B�B� B�+B�+B�7B�DB�JB�DB�1B�JB�hB�bB�bB�bB�uB�oB�PB�%B�oB��B��B��B�B�B�B��B��B��B�B�'B�3B�LB�^B�^B�qB�}BĜBŢB��BɺB��B��B��B��B�B�
B�B�B�/B�5B�/B�#B�B�NB�ZB�TB�B�B�B�B�B�B�B��B	B	%B	B	B	B		7B	JB	DB	PB	hB	�B	 �B	�B	!�B	!�B	$�B	$�B	2-B	49B	5?B	8RB	>wB	G�B	I�B	G�B	G�B	N�B	P�B	R�B	W
B	W
B	XB	ZB	VB	\)B	e`B	iyB	l�B	m�B	l�B	m�B	o�B	p�B	q�B	s�B	s�B	t�B	t�B	w�B	y�B	|�B	~�B	�%B	�7B	�+B	�DB	�JB	�oB	�uB	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�3B	�9B	�FB	�RB	�XB	�jB	�jB	�qB	�qB	�qB	�qB	�jB	�^B	��B	ÖB	B	ÖB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�#B	�B	�B	�B	�B	�/B	�5B	�;B	�BB	�5B	�BB	�ZB	�TB	�NB	�HB	�BB	�TB	�TB	�fB	�fB	�mB	�fB	�fB	�sB	�sB	�yB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
%B
+B
	7B
1B
1B
	7B
	7B

=B
DB
DB
DB
DB
DB

=B

=B
	7B
1B
%B
1B
\B
bB
bB
hB
hB
hB
bB
oB
uB
oB
oB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
!�B
 �B
!�B
 �B
�B
�B
�B
�B
 �B
"�B
"�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
'�B
)�B
)�B
+B
+B
+B
)�B
'�B
,B
-B
,B
,B
-B
-B
.B
-B
-B
/B
/B
/B
/B
/B
0!B
1'B
2-B
2-B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
5?B
5?B
5?B
49B
49B
33B
2-B
33B
49B
5?B
49B
49B
49B
49B
5?B
49B
7LB
6FB
6FB
9XB
9XB
9XB
:^B
;dB
:^B
:^B
9XB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
;dB
<jB
=qB
=qB
=qB
>wB
=qB
=qB
>wB
=qB
>wB
=qB
?}B
@�B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
A�B
A�B
A�B
@�B
A�B
E�B
D�B
C�B
D�B
D�B
E�B
D�B
C�B
D�B
C�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
H�B
J�B
J�B
J�B
K�B
K�B
K�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
K�B
L�B
K�B
L�B
K�B
K�B
L�B
N�B
N�B
N�B
M�B
L�B
M�B
N�B
O�B
O�B
O�B
N�B
O�B
O�B
Q�B
P�B
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
R�B
S�B
VB
W
B
W
B
VB
VB
VB
VB
VB
T�B
W
B
W
B
W
B
W
B
VB
VB
W
B
W
B
XB
YB
XB
XB
XB
ZB
[#B
[#B
ZB
ZB
ZB
[#B
[#B
ZB
ZB
ZB
[#B
\)B
\)B
[#B
[#B
ZB
[#B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
]/B
^5B
^5B
_;B
^5B
^5B
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
aHB
`BB
bNB
cTB
cTB
aHB
bNB
bNB
cTB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
dZB
dZB
e`B
gmB
ffB
gmB
gmB
ffB
gmB
gmB
hsB
gmB
gmB
hsB
iyB
jB
jB
jB
jB
jB
jB
iyB
jB
k�B
k�B
jB
jB
jB
l�B
l�B
m�B
m�B
m�B
l�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B��B��B��B�B�B�B��B��B�B��B�B��B�B��B��B� B� B� B��B��B� B� B��B��B��B��B��B��B��B�B��B��B��B�8B�CB}<Bx�Bn}Bq�BdtBO(B+�B1B�B �B�B��B�oB�'Bo�B�	B��B��B~wB^B,�B&�B)�B"4B)B
��B[B iB
��B
�B
�B
��B
�HB
ңB
ϑB
�=B
�}B
�/B
��B
�iB
��B
��B
�B
}B
fB
@�B
6zB
$�B
1�B
,qB
�B

�B	�.B

	B

�B
�B	��B	��B	�B	��B	�[B	�,B	��B	�IB	�B	��B	��B	�:B	�B	��B	��B	��B	��B	��B	� B	p�B	q�B	iB	c�B	gRB	bNB	[=B	U2B	C�B	N�B	O�B	N�B	K�B	H�B	I�B	D�B	CaB	@OB	8�B	)�B	�B	qB		B	�B	�B	B	HB	�B��B��B�B��B��B�B�B�LB�8B�[B��B�6B�FB��B��B�-BϑB�B�B�dBרBҽBοB̳BȴB�XB�XB�RBB�B�LB��B�RB�UB��B��B��B��B�B��B�wB��B��B��B��B��B��B�PB�:B�JB��B�B��B��B��B��B��Bx�Bx�Bb�Bs�Bt�Bv`BxBpoBf�BY�B`\BPbB[�BY1BW$BQNB<�BN�BF�B>�B@iB<B>�B<�B:�B8�B;JB8RB5?B,�B,�B3�B4B:�B<�B=�B=�B;�B9�B3�B+BjB)�B*�B)�B(�B,�B+�B+�B$�B1B vB'RB$tB �BpB�B�BMB1B
	BUBjBSB�B��B
B#�B+6B'mB!�B�B�BSB\B
�B�B�BQBdB"NB \B!BQB	7B �B#B&LB$@B5BMB�B  B�B�B �B&fB'mB#�B �B1[B1�B-�B/�B)�B)�B4�B7�B;�B;�B8�B8�B;�B6�B5�B=�B8B8�B4TB;B@ B@ B?HB?.BD3BF?BI7BJXBJXBLdBO�BO�BP�BN�BS�B[�B[�B`�B_�B_�Bb�Be�Bf�Bf�Bf�Bk�Bj�Bk�Bl�Bl�BgRBk6Bu�BtBr-Bq[BpoBx8BzxB.B�;B�UB�{B��B�_B��B��B��B�~B��B��B��B��B��B��B��B��B��B��B�EB�@B�5B�&B�RB�WB�CB�=B�_B�eB��B��B��B��B��B��B��B��B��B�B�%B�B�=B�0B�(B�&B�[B�SB�YB�eB�B�IB�jB�dBیB��B�B��B�B��B��B��B��B��B��B�B�B	�B	YB	mB	�B	�B		�B	~B	�B	�B	�B	�B	 �B	 B	"B	"4B	%`B	%�B	2aB	4�B	5�B	8�B	>�B	G�B	I�B	G�B	H1B	OB	QB	S@B	W?B	WYB	X_B	ZQB	V�B	\�B	e�B	i�B	l�B	m�B	l�B	m�B	o�B	p�B	q�B	s�B	s�B	uB	uB	xB	z*B	}"B	HB	�tB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�$B	�$B	�$B	�>B	�0B	�"B	�=B	�)B	�]B	�;B	�AB	�GB	�9B	�hB	�TB	�zB	�lB	��B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�B	�6B	��B	�B	�B	�BB	�HB	�B	�#B	�WB	�=B	�QB	�EB	�_B	�B	�dB	�jB	�pB	�\B	�jB	�\B	�ZB	�nB	�B	�|B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	�B	��B	��B	�B	��B	��B	�B	�B	��B	��B	��B	��B	�/B	��B	�	B	��B	��B	�9B	�B	��B	�	B	�RB	�*B	�6B	�.B
 B
'B
GB
'B
[B
GB
[B
gB
YB
EB
	RB
fB
fB
	lB
	lB

rB
^B
DB
DB
xB
DB

rB

XB
	RB
fB
�B
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
!�B
 �B
!�B
 �B
�B
�B
�B
B
!B
"�B
# B
'B
(
B
($B
(
B
($B
)*B
)*B
)B
($B
*B
*0B
+6B
+6B
+6B
*0B
(>B
,=B
-)B
,=B
,=B
-)B
-CB
./B
-]B
-CB
/OB
/OB
/OB
/OB
/5B
0UB
1[B
2-B
2GB
1[B
2GB
2GB
2GB
3MB
3MB
3hB
4TB
5ZB
5ZB
5ZB
49B
4TB
3hB
2aB
3MB
4nB
5ZB
4nB
4nB
4nB
4nB
5tB
4�B
7�B
6�B
6�B
9rB
9�B
9rB
:xB
;dB
:�B
:�B
9�B
;�B
<�B
<�B
<�B
<jB
<�B
<�B
;�B
<�B
=�B
=�B
=�B
>�B
=�B
=�B
>�B
=�B
>�B
=�B
?�B
@�B
?�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
A�B
A�B
A�B
@�B
A�B
E�B
D�B
C�B
D�B
D�B
E�B
D�B
C�B
D�B
C�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
H�B
H�B
J�B
J�B
J�B
K�B
K�B
K�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
K�B
MB
K�B
MB
K�B
K�B
MB
N�B
N�B
N�B
NB
MB
M�B
OB
PB
PB
O�B
OB
P.B
PB
R B
Q B
RB
R B
RB
S&B
R:B
R:B
S&B
T,B
VB
W$B
W
B
VB
VB
V9B
VB
VB
UB
W
B
W
B
W$B
W?B
V9B
V9B
W$B
W$B
XEB
Y1B
XEB
XEB
XEB
ZQB
[=B
[=B
ZQB
ZQB
ZB
[=B
[#B
Z7B
ZQB
Z7B
[WB
\)B
\CB
[WB
[WB
ZkB
[=B
\]B
\CB
]dB
^OB
^OB
^OB
^5B
^OB
^OB
^OB
^OB
^jB
]IB
^jB
^OB
_;B
^jB
^jB
_VB
_VB
_VB
_VB
_pB
_VB
`\B
a|B
abB
abB
abB
a|B
`vB
bhB
cTB
cTB
a|B
bhB
bhB
c�B
cnB
dtB
e`B
ezB
e`B
ezB
ezB
ezB
e`B
e`B
ezB
ezB
ezB
e`B
ezB
e�B
dtB
dtB
ezB
g�B
f�B
g�B
g�B
f�B
g�B
g�B
hsB
g�B
g�B
h�B
i�B
j�B
jB
jB
j�B
j�B
j�B
i�B
j�B
k�B
k�B
j�B
j�B
j�B
l�B
l�B
m�B
m�B
m�B
l�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.12(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803160033452018031600334520180316003345201806221238542018062212385420180622123854201804050435572018040504355720180405043557  JA  ARFMdecpA19c                                                                20180312093527  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180312003528  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180312003530  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180312003531  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180312003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180312003531  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180312003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180312003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180312003532  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180312003532                      G�O�G�O�G�O�                JA  ARUP                                                                        20180312005543                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180312154729  CV  JULD            G�O�G�O�F�                JM  ARCAJMQC2.0                                                                 20180315153345  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180315153345  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193557  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033854  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                