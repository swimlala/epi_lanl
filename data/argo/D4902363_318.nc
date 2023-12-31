CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-01-06T00:36:31Z creation;2019-01-06T00:36:36Z conversion to V3.1;2019-12-19T07:24:02Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  20190106003631  20200115131516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              >A   JA  I2_0576_318                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؝�|e� 1   @؝�q��@9��s��dF�g��	1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D<��D=y�D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�<�Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�C3DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D���D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��\@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�=qB�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD��DxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD��DxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD~�D�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<��D=q�D=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDi~�Di�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�8�D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�?\D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D���D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�\D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�\D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��\D�<)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�r�A�p�A�t�A�v�A�t�A�t�A�x�A�x�A�x�A�v�A�v�A�x�A�z�A�|�A��PA���A��A�~�A��+A��A��A�v�A�n�A�`BA�M�A�C�A�;dA�33A�+A� �A��A��A��A�
=A���A��`A��/A��
A��jA���A���A���A���A��DA�=qA���A��9A�bNA�ĜA�1'A�/A��A��\A���A��A�9XA��A��
A�
=A��A��A�bNA� �A���A�
=A�9XA�1A�A���A��PA�=qA���A���A�%A��FA�t�A��/A��A�+A�|�A���A�ffA�VA��A��+A�l�A�5?A���A��FA�v�A�oA��A�p�A��A�  A��`A��PA�x�A��\A�+A�VA��+A��HA��PA�O�A�"�A�l�A���A��`A�VA���A�{A�ffA���A��A���A�(�A�5?At�A~-A}XA|��A{��Az��Az$�Ay?}Awp�AtQ�AqG�An�/AnbAm`BAl��AjZAh-Ae�
AdJAc`BAa�7A_��A^��A\�`A[�TAZI�AYK�AW��AUS�AT5?AT(�ATJASx�ASoAR��ARbAQ��AP��AP�AOO�AM�#AL��AKƨAJ��AJffAJbAIAI�AH�yAH5?AF��AE�7AD�+AC�;ABffA@�RA>r�A<ĜA<JA;`BA:��A:$�A8�HA7�;A6 �A4�!A4A3x�A2A�A1oA0Q�A0�A/��A/hsA.�jA-S�A,��A,{A+C�A*jA)�#A(��A'�A&��A%�#A$��A$VA$5?A#��A#&�A"��A!��A �A��A"�A%A�A��AO�AoA�yAoA�
A��A/A��A
=A��A��A�TA�hA
=A�7A��AI�A33A�uAO�A
�A	�FA	"�AȴA-A�
A�A��A��AffAA�A��A�A��A1A �`@��\@��@�hs@�hs@�`B@�`B@�O�@��@���@��+@�  @���@��
@��@��@���@���@�Ĝ@��@홚@� �@�\@���@�@�Q�@��@߮@�\)@�+@��H@ާ�@�{@��@�9X@�C�@�M�@ٲ-@�p�@���@׶F@պ^@Դ9@���@�33@ҏ\@�$�@��T@ёh@�O�@�V@��@���@ЋD@ϕ�@�1@�/@�(�@ǥ�@��@Ƨ�@Ƈ+@���@�1@��#@��@�33@��@��P@�@��u@��@�E�@���@���@��7@�O�@�&�@��`@��@�Z@�ƨ@�ȴ@�5?@��^@�/@��u@�I�@�dZ@�-@���@��`@�1'@�K�@�+@��@�o@�@���@�$�@���@�x�@�%@���@�Ĝ@���@��/@���@���@�?}@��@��@��@��w@��@��P@�dZ@��@�{@�hs@�%@�j@��w@��H@�{@��h@���@�E�@���@��D@���@��H@���@���@���@��+@��+@�v�@�E�@��@���@�O�@�%@�z�@���@�t�@�33@��@�V@�&�@�z�@�I�@�9X@�(�@���@�ff@���@�p�@�?}@�%@�Ĝ@�1@�ƨ@�|�@�K�@�"�@�o@�
=@���@�V@���@��@�&�@��/@��D@�bN@�(�@�1@�  @��m@���@�t�@�33@�$�@��@���@�`B@��@��@�z�@�1'@���@�K�@��@�ȴ@��R@���@��+@�$�@��#@���@��9@��@�A�@��m@�"�@�=q@�{@��@���@���@��7@�p�@�X@�G�@��@��@���@��j@���@��D@�z�@�Q�@�(�@� �@��@�b@�P@~{@|�@|z�@|�@|9X@|1@{�
@{��@{"�@y��@x��@x�9@x��@x�u@xr�@xr�@w��@v��@w+@x  @w�@u@uO�@t�@tz�@tz�@s�
@r�@r�!@r�\@r=q@q��@qx�@qhs@q��@q��@q7L@p�`@pQ�@o��@o�w@ol�@n�+@n@m�h@m�@mO�@mO�@m/@lj@k33@j�\@jM�@jJ@i��@i��@i�7@i%@hĜ@h1'@hb@g��@g�P@g
=@fff@e�@e��@e?}@dz�@ct�@c@b�@b�H@b��@b��@b��@b~�@b^5@a�@a��@aG�@`��@`bN@`A�@`1'@`1'@` �@_�@_�;@_�;@_�;@_�@^�+@]�@\��@\�D@\9X@\(�@\(�@\(�@\(�@\�@[��@[t�@[33@Z��@Z��@Z=q@Y�7@X�9@Xb@W�@W�;@W�;@W�w@W�P@WK�@V��@VV@U�T@U�-@U��@UV@S��@SS�@S33@S33@S"�@S"�@S"�@R�@R��@R=q@Q�#@Q��@P��@O��@Ol�@N��@M@M`B@M?}@M/@M�@MV@L�@Lj@L1@K�@Kt�@KC�@KC�@J�@J��@J��@J��@J�!@J-@I�#@I&�@H��@H1'@G�@G��@Fff@E�@E�T@E��@E��@E�@EO�@E/@D��@D�@D�/@D�@C�F@CdZ@CS�@B�@B�!@BM�@A�#@Ahs@AG�@A7L@@��@@�u@@bN@@ �@?�w@?l�@>��@>��@>v�@>v�@>$�@=��@=��@=@=@=@=@=@=��@=�@<�j@<�D@<j@<Z@;�m@;��@;dZ@:�@:~�@:=q@:�@9�#@9��@9x�@9hs@9X@9X@97L@8Ĝ@81'@8 �@8 �@8 �@7�;@7\)@7\)@7K�@6�@6E�@6@5��@5@5�-@5��@5�h@5`B@4�/@4�@4z�@3t�@2�H@2��@2�H@2�H@2�@2�@2n�@2M�@2-@1�#@1x�@0�9@/�;@/�w@/�;@/��@/\)@/K�@/;d@/
=@.�@.ȴ@.�R@.��@.v�@-�@-�-@-�@-?}@-V@,�j@,��@,I�@+�F@+t�@+33@+o@+@+@*�H@*��@*�@)x�@)X@)7L@)7L@)&�@)�@(Ĝ@(��@(�u@(r�@(bN@(Q�@( �@&�y@&E�@%��@%?}@%V@$�@$��@$z�@$j@$9X@$�@#�m@#��@#C�@"�@"��@"�\@"�\@"�\@"^5@!��@!��@!G�@!�@ Ĝ@ r�@ Q�@�@|�@\)@;d@+@�@��@�@��@@�h@/@��@��@Z@(�@��@ƨ@�@33@@��@^5@=q@��@�#@�#@�#@��@x�@�@��@��@Ĝ@��@�@bN@Q�@A�@  @�w@�P@\)@;d@
=@��@�y@�R@5?@{@�@��@p�@?}@/@V@�@�j@��@��@��@(�@ƨ@�F@��@�@t�@S�@"�@@�H@�\@^5@-@��@�#@�^@��@�7@7L@%@�`@�9@r�@1'@�;@�w@��@l�@�@��@�@�R@��@��@��@��@ff@$�@{@�@O�@��@�@9X@1@1@1@��@��@��@�
@��@S�@
��@
M�@
=q@
=q@
=q@
-@
-@
-@	�@	�#@	�#@	��@	��@	��@	�7@	X@	�@	%@��@�9@r�@bN@A�@ �@��@�@�P@+@�y@�@�R@�+@V@$�@�@@�-@��@�h@�h@p�@O�@?}@/@V@��@��@�j@�j@�@z�@Z@I�@9X@�@1@�
@��@dZ@"�@�!@~�@n�@n�@^5@=q@-@-@-@�@�#@��@��@�7@�7@x�@hs@hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�r�A�p�A�t�A�v�A�t�A�t�A�x�A�x�A�x�A�v�A�v�A�x�A�z�A�|�A��PA���A��A�~�A��+A��A��A�v�A�n�A�`BA�M�A�C�A�;dA�33A�+A� �A��A��A��A�
=A���A��`A��/A��
A��jA���A���A���A���A��DA�=qA���A��9A�bNA�ĜA�1'A�/A��A��\A���A��A�9XA��A��
A�
=A��A��A�bNA� �A���A�
=A�9XA�1A�A���A��PA�=qA���A���A�%A��FA�t�A��/A��A�+A�|�A���A�ffA�VA��A��+A�l�A�5?A���A��FA�v�A�oA��A�p�A��A�  A��`A��PA�x�A��\A�+A�VA��+A��HA��PA�O�A�"�A�l�A���A��`A�VA���A�{A�ffA���A��A���A�(�A�5?At�A~-A}XA|��A{��Az��Az$�Ay?}Awp�AtQ�AqG�An�/AnbAm`BAl��AjZAh-Ae�
AdJAc`BAa�7A_��A^��A\�`A[�TAZI�AYK�AW��AUS�AT5?AT(�ATJASx�ASoAR��ARbAQ��AP��AP�AOO�AM�#AL��AKƨAJ��AJffAJbAIAI�AH�yAH5?AF��AE�7AD�+AC�;ABffA@�RA>r�A<ĜA<JA;`BA:��A:$�A8�HA7�;A6 �A4�!A4A3x�A2A�A1oA0Q�A0�A/��A/hsA.�jA-S�A,��A,{A+C�A*jA)�#A(��A'�A&��A%�#A$��A$VA$5?A#��A#&�A"��A!��A �A��A"�A%A�A��AO�AoA�yAoA�
A��A/A��A
=A��A��A�TA�hA
=A�7A��AI�A33A�uAO�A
�A	�FA	"�AȴA-A�
A�A��A��AffAA�A��A�A��A1A �`@��\@��@�hs@�hs@�`B@�`B@�O�@��@���@��+@�  @���@��
@��@��@���@���@�Ĝ@��@홚@� �@�\@���@�@�Q�@��@߮@�\)@�+@��H@ާ�@�{@��@�9X@�C�@�M�@ٲ-@�p�@���@׶F@պ^@Դ9@���@�33@ҏ\@�$�@��T@ёh@�O�@�V@��@���@ЋD@ϕ�@�1@�/@�(�@ǥ�@��@Ƨ�@Ƈ+@���@�1@��#@��@�33@��@��P@�@��u@��@�E�@���@���@��7@�O�@�&�@��`@��@�Z@�ƨ@�ȴ@�5?@��^@�/@��u@�I�@�dZ@�-@���@��`@�1'@�K�@�+@��@�o@�@���@�$�@���@�x�@�%@���@�Ĝ@���@��/@���@���@�?}@��@��@��@��w@��@��P@�dZ@��@�{@�hs@�%@�j@��w@��H@�{@��h@���@�E�@���@��D@���@��H@���@���@���@��+@��+@�v�@�E�@��@���@�O�@�%@�z�@���@�t�@�33@��@�V@�&�@�z�@�I�@�9X@�(�@���@�ff@���@�p�@�?}@�%@�Ĝ@�1@�ƨ@�|�@�K�@�"�@�o@�
=@���@�V@���@��@�&�@��/@��D@�bN@�(�@�1@�  @��m@���@�t�@�33@�$�@��@���@�`B@��@��@�z�@�1'@���@�K�@��@�ȴ@��R@���@��+@�$�@��#@���@��9@��@�A�@��m@�"�@�=q@�{@��@���@���@��7@�p�@�X@�G�@��@��@���@��j@���@��D@�z�@�Q�@�(�@� �@��@�b@�P@~{@|�@|z�@|�@|9X@|1@{�
@{��@{"�@y��@x��@x�9@x��@x�u@xr�@xr�@w��@v��@w+@x  @w�@u@uO�@t�@tz�@tz�@s�
@r�@r�!@r�\@r=q@q��@qx�@qhs@q��@q��@q7L@p�`@pQ�@o��@o�w@ol�@n�+@n@m�h@m�@mO�@mO�@m/@lj@k33@j�\@jM�@jJ@i��@i��@i�7@i%@hĜ@h1'@hb@g��@g�P@g
=@fff@e�@e��@e?}@dz�@ct�@c@b�@b�H@b��@b��@b��@b~�@b^5@a�@a��@aG�@`��@`bN@`A�@`1'@`1'@` �@_�@_�;@_�;@_�;@_�@^�+@]�@\��@\�D@\9X@\(�@\(�@\(�@\(�@\�@[��@[t�@[33@Z��@Z��@Z=q@Y�7@X�9@Xb@W�@W�;@W�;@W�w@W�P@WK�@V��@VV@U�T@U�-@U��@UV@S��@SS�@S33@S33@S"�@S"�@S"�@R�@R��@R=q@Q�#@Q��@P��@O��@Ol�@N��@M@M`B@M?}@M/@M�@MV@L�@Lj@L1@K�@Kt�@KC�@KC�@J�@J��@J��@J��@J�!@J-@I�#@I&�@H��@H1'@G�@G��@Fff@E�@E�T@E��@E��@E�@EO�@E/@D��@D�@D�/@D�@C�F@CdZ@CS�@B�@B�!@BM�@A�#@Ahs@AG�@A7L@@��@@�u@@bN@@ �@?�w@?l�@>��@>��@>v�@>v�@>$�@=��@=��@=@=@=@=@=@=��@=�@<�j@<�D@<j@<Z@;�m@;��@;dZ@:�@:~�@:=q@:�@9�#@9��@9x�@9hs@9X@9X@97L@8Ĝ@81'@8 �@8 �@8 �@7�;@7\)@7\)@7K�@6�@6E�@6@5��@5@5�-@5��@5�h@5`B@4�/@4�@4z�@3t�@2�H@2��@2�H@2�H@2�@2�@2n�@2M�@2-@1�#@1x�@0�9@/�;@/�w@/�;@/��@/\)@/K�@/;d@/
=@.�@.ȴ@.�R@.��@.v�@-�@-�-@-�@-?}@-V@,�j@,��@,I�@+�F@+t�@+33@+o@+@+@*�H@*��@*�@)x�@)X@)7L@)7L@)&�@)�@(Ĝ@(��@(�u@(r�@(bN@(Q�@( �@&�y@&E�@%��@%?}@%V@$�@$��@$z�@$j@$9X@$�@#�m@#��@#C�@"�@"��@"�\@"�\@"�\@"^5@!��@!��@!G�@!�@ Ĝ@ r�@ Q�@�@|�@\)@;d@+@�@��@�@��@@�h@/@��@��@Z@(�@��@ƨ@�@33@@��@^5@=q@��@�#@�#@�#@��@x�@�@��@��@Ĝ@��@�@bN@Q�@A�@  @�w@�P@\)@;d@
=@��@�y@�R@5?@{@�@��@p�@?}@/@V@�@�j@��@��@��@(�@ƨ@�F@��@�@t�@S�@"�@@�H@�\@^5@-@��@�#@�^@��@�7@7L@%@�`@�9@r�@1'@�;@�w@��@l�@�@��@�@�R@��@��@��@��@ff@$�@{@�@O�@��@�@9X@1@1@1@��@��@��@�
@��@S�@
��@
M�@
=q@
=q@
=q@
-@
-@
-@	�@	�#@	�#@	��@	��@	��@	�7@	X@	�@	%@��@�9@r�@bN@A�@ �@��@�@�P@+@�y@�@�R@�+@V@$�@�@@�-@��@�h@�h@p�@O�@?}@/@V@��@��@�j@�j@�@z�@Z@I�@9X@�@1@�
@��@dZ@"�@�!@~�@n�@n�@^5@=q@-@-@-@�@�#@��@��@�7@�7@x�@hs@hs1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B]/B]/B]/B\)B]/B]/B]/B^5B`BBcTBdZBe`BhsBk�Bu�By�B~�B� B}�B}�B|�B~�B� B�B�%B�1B�=B�DB�DB�JB�JB�JB�DB�DB�DB�VB�VB�JB�VB�bB�bB�PB�1B~�Bw�B{�Bn�B[#B>wBJ�BH�BF�BT�BA�B!�B9XB�B-B2-B]/Be`BbNBXBE�B2-BB!�B-B5?B#�B1B��B+B
=B%B�B�qB��B��B�qB��BȴBÖBŢBB�LB��B�JBn�B}�Be`B@�BaHBe`B]/BJ�B+B�BB1B
��B
��B
��B
��B
�B
�B
�B
��B
�B
��B
��B
�VB
�B
hsB
_;B
dZB
YB
R�B
O�B
M�B
L�B
E�B
33B
49B
"�B
B	�B	��B	��B	�BB	�B	ȴB	��B	�oB	�PB	�bB	��B	�B	x�B	s�B	o�B	jB	cTB	bNB	W
B	H�B	S�B	m�B	iyB	^5B	^5B	XB	Q�B	O�B	C�B	?}B	6FB	&�B	(�B	,B	/B	0!B	49B	1'B	.B	"�B	�B	B	B��B��B�fB�
B��B��B�#B�B��B��B�jB�FB��B��B�!B�B��B��B��B��B��B��B��B�B��B�{B�=B�B~�Br�Bn�BffBdZBiyBiyBq�BgmBdZB_;BT�BT�BO�BYBcTBZBQ�B[#BYBP�B:^B8RBS�BL�BF�B7LBJ�BF�BK�B=qB�B�B49B/B �B%B �B-B.B33B6FB33B49B5?B.B49B2-B%�B�B �B �B �B �BoBDB$�B5?B5?B49B33B/B(�B�BVB
=BuBDB�B&�B.B-B#�B�BBPB
=B��B�mB  B(�B,B+B+B(�B&�B!�B!�B �B"�B"�B'�B+B%�B �B�B.B6FB6FB9XB>wB?}B?}B?}B>wB=qB9XB1'B!�BhB�B:^BA�BA�BG�BD�B:^B/B)�B;dB33B.BA�B@�BH�BI�BVB^5BaHBbNBcTBe`Be`BffBffBbNBcTBgmBiyBjBjBn�BiyBjBw�Bu�B|�B�B�VB�\B�VB�PB�=B�PB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�'BB�qB��BŢB��B��B��B�B�
B�B��B��B��B��B�
B�B�B�
B�)B�B�B�B�5B�`B�fB�ZB�5B�#B�yB�B�B�B��B�B��B��B	B	B	B	B	B	B	%B	VB	�B	�B	�B	�B	�B	�B	!�B	 �B	�B	 �B	 �B	�B	%�B	%�B	'�B	(�B	)�B	)�B	-B	.B	33B	7LB	;dB	=qB	=qB	=qB	;dB	=qB	<jB	H�B	K�B	J�B	J�B	K�B	P�B	aHB	cTB	dZB	e`B	gmB	hsB	iyB	jB	jB	k�B	m�B	n�B	o�B	p�B	q�B	q�B	r�B	u�B	u�B	t�B	q�B	q�B	x�B	~�B	�B	�B	�%B	�+B	�+B	�%B	�B	�1B	�JB	�\B	�\B	�\B	�\B	�PB	�\B	��B	��B	�uB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�-B	�!B	�3B	�9B	�LB	�FB	�FB	�?B	�-B	�B	�FB	�XB	�^B	�^B	�dB	�^B	�RB	�^B	�^B	�wB	�wB	�wB	�wB	��B	ÖB	ĜB	ŢB	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�5B	�5B	�HB	�HB	�HB	�HB	�BB	�;B	�5B	�BB	�BB	�NB	�BB	�BB	�NB	�fB	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
+B
+B
1B
+B
1B

=B
	7B
1B
%B
%B
%B
+B

=B
DB

=B
1B
\B
uB
uB
oB
uB
uB
{B
{B
�B
{B
uB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
"�B
"�B
!�B
!�B
 �B
�B
�B
!�B
"�B
"�B
 �B
!�B
"�B
!�B
"�B
$�B
&�B
%�B
&�B
'�B
'�B
'�B
'�B
&�B
$�B
%�B
)�B
)�B
)�B
(�B
'�B
+B
+B
(�B
(�B
,B
-B
-B
-B
-B
-B
,B
+B
-B
-B
+B
.B
2-B
33B
33B
33B
2-B
0!B
33B
49B
33B
2-B
1'B
2-B
6FB
9XB
9XB
7LB
:^B
:^B
9XB
9XB
:^B
:^B
:^B
9XB
7LB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
9XB
=qB
=qB
>wB
?}B
?}B
=qB
=qB
;dB
;dB
@�B
@�B
A�B
A�B
@�B
@�B
@�B
A�B
A�B
A�B
@�B
>wB
:^B
=qB
?}B
B�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
D�B
D�B
C�B
E�B
F�B
F�B
G�B
G�B
E�B
D�B
G�B
G�B
H�B
G�B
H�B
I�B
I�B
J�B
L�B
L�B
M�B
M�B
L�B
L�B
K�B
I�B
K�B
L�B
M�B
O�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
P�B
R�B
R�B
S�B
T�B
S�B
S�B
R�B
Q�B
T�B
T�B
T�B
T�B
VB
VB
VB
T�B
T�B
T�B
VB
VB
W
B
W
B
XB
W
B
VB
T�B
XB
YB
XB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
YB
YB
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
^5B
^5B
^5B
_;B
^5B
]/B
^5B
_;B
^5B
^5B
_;B
_;B
`BB
aHB
`BB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
bNB
aHB
cTB
bNB
_;B
bNB
cTB
cTB
e`B
gmB
gmB
gmB
gmB
gmB
ffB
e`B
dZB
dZB
ffB
iyB
jB
jB
jB
jB
iyB
hsB
iyB
jB
jB
iyB
jB
iyB
iyB
iyB
jB
jB
iyB
iyB
k�B
k�B
jB
jB
jB
k�B
jB
k�B
m�B
l�B
l�B
l�B
m�B
m�B
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
q�B
q�B
q�B
p�B
p�B
q�B
q�B
q�B
p�B
q�B
p�B
o�B
p�B
p�B
p�B
r�B
s�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
u�B
u�B
v�B
v�B
v�B
w�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B]/B]/B]IB\CB]/B]IB]/B^OB`\BcTBdZBe`BhsBk�Bu�By�B~�B� B~B~B}BB�4B�GB�?B�KB�XB�^B�^B�JB�dB�dB�xB�^B�xB�pB�pB��B��B�}B�}B�jB��B�Bx�B|jBo�B]BBBM6BK�BI�BV�BESB&�B<jB �B0�B5�B]�BfBcBYBG�B4�B
#B#�B-�B5�B%BDB��B�BB�B��B��B��B��B��B�HBɆB�gB��B��B�8B�zB��Bq�BHBh$BC�Ba�Be�B]�BL0B-�BB?B
#B �B
�]B
��B
��B
�B
�B
��B
�kB
��B
�yB
�B
�.B
��B
kQB
a�B
e�B
[#B
TaB
QNB
N�B
M�B
F�B
4�B
5B
$ZB
�B	��B	�{B	ΊB	�-B	�7B	�=B	��B	�gB	�.B	�oB	��B	��B	{B	uZB	q�B	l"B	e`B	c�B	YKB	KxB	UMB	mwB	i�B	_B	^�B	X�B	R�B	P�B	D�B	@�B	7�B	(�B	*�B	-CB	0B	0�B	4�B	1�B	.�B	#�B	�B	B	tB�}B�"B�B�BңB�B�)B�#B��B��B�(B��B�DB��B�'B�!B��B�NB��B�KB��B��B��B�+B��B��B��B�YB�BtBo�Bh
Be�Bj�BjeBq�BhsBe,B`BBVmBVBQ�BY�Bc�B[	BS&B[�BY�BQ�B<�B:*BTaBM�BG�B9rBK^BG�BLJB>wBB!�B5?B0;B"�B	�B"NB.IB/OB4B6�B4B4�B5�B/B4�B2�B'BB!�B!�B!�B!�B,BB%zB5?B5tB4nB3MB/OB)yB vB�B�BaB6B�B'�B.IB-)B$@BB�BpBxB��B�B�B(�B,WB+kB+6B)_B'8B"hB"�B!�B#�B#�B(sB+kB&�B!�B!B.�B6�B6�B9�B>�B?�B?�B?�B>�B=�B9�B1�B#B�B�B:�BBBB'BG�BD�B;JB0�B+�B<B4�B0BBuBA�BI�BJ�BV�B^�Ba|Bb�Bc�Be�Be�Bf�Bf�BcBd&Bg�Bi�BkBkBoBjKBk�Bx8BvzB}�B��B�pB��B�pB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�)B�!B�4B��B�,B� B�;B�OB��B�DB�fB��B��B��B��B�NB�-B��B�wB�UB�?B� B�,B�B�B�?B�9B�2B�MB�FB�aB�YB֡BؓB�sB�]BڠBٴB��BޞB�zB�B�B��B�)B��B��B��B�B�B�hB�"B�.B	AB	MB	SB	SB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	!B	 B	!B	!B	kB	&B	&2B	(>B	)DB	*KB	*B	-]B	.�B	3�B	7�B	;�B	=�B	=�B	=�B	;�B	=�B	="B	H�B	LB	K)B	KDB	L~B	Q�B	aHB	c�B	d�B	e�B	g�B	h�B	i�B	j�B	j�B	k�B	m�B	n�B	o�B	p�B	q�B	q�B	r�B	u�B	u�B	t�B	r-B	raB	y>B	HB	�'B	�B	�YB	�_B	�zB	�tB	��B	��B	�dB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�#B	��B	��B	�&B	� B	�B	�$B	�B	�)B	�kB	�IB	�OB	�oB	�MB	�aB	��B	��B	��B	�LB	�`B	�zB	�tB	�|B	��B	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	� B	� B	�B	�B	�B	�B	�:B	�:B	�&B	�B	�+B	�1B	�B	�1B	�EB	�B	�B	�+B	�SB	�oB	ՁB	�QB	�jB	�jB	�bB	�HB	�bB	�HB	�\B	�pB	�jB	�\B	�vB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�9B	�?B	�B	�0B	�XB
;B
B
B
MB
3B
GB
UB
aB
gB
+B
_B
KB
EB
KB

XB
	RB
KB
tB
YB
�B
zB

rB
xB

rB
�B
vB
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
!�B
"�B
"�B
!�B
!�B
 �B
�B
�B
!�B
"�B
"�B
 �B
!�B
"�B
!�B
# B
$�B
'B
&B
'B
($B
(
B
'�B
(
B
'B
%B
&B
)�B
*B
*B
)*B
($B
+B
+6B
)DB
)DB
,=B
-CB
-)B
-B
-)B
-)B
,=B
+6B
-)B
-)B
+�B
.cB
2GB
33B
33B
3MB
2aB
0UB
3MB
4nB
3hB
2aB
1�B
2�B
6FB
9rB
9rB
7�B
:^B
:�B
9rB
9rB
:xB
:^B
:�B
9�B
7�B
9rB
:�B
:�B
:xB
:xB
;B
;�B
9�B
=�B
=�B
>�B
?}B
?�B
=�B
=�B
;�B
;�B
@�B
@�B
A�B
A�B
@�B
@�B
@�B
A�B
A�B
A�B
@�B
>�B
:�B
=�B
?�B
B�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
D�B
D�B
C�B
E�B
F�B
F�B
G�B
G�B
E�B
D�B
G�B
G�B
H�B
G�B
H�B
I�B
J	B
KB
L�B
MB
M�B
M�B
MB
MB
K�B
J#B
LB
MB
M�B
PB
OB
PB
O�B
O�B
O�B
QB
Q B
R B
Q4B
S&B
S&B
TB
T�B
S�B
TB
S&B
R B
UB
UB
UB
UB
VB
VB
VB
UB
U2B
U2B
V9B
V9B
W?B
W?B
X+B
W?B
VB
U2B
X+B
YKB
XEB
Y1B
ZQB
Z7B
ZQB
ZQB
ZQB
[=B
[#B
[=B
YKB
YKB
\CB
\CB
\)B
\CB
\CB
\]B
\]B
\]B
\CB
]dB
]dB
^OB
^OB
^jB
_VB
^OB
]IB
^OB
_pB
^OB
^OB
_pB
_pB
`vB
a|B
`\B
abB
bhB
b�B
cnB
cTB
cnB
cTB
cTB
bhB
abB
cnB
b�B
_�B
b�B
c�B
c�B
e�B
g�B
g�B
gmB
g�B
gmB
f�B
ezB
d�B
d�B
f�B
iyB
j�B
j�B
jB
j�B
i�B
h�B
i�B
jB
j�B
i�B
j�B
i�B
i�B
i�B
j�B
j�B
i�B
i�B
k�B
k�B
j�B
j�B
j�B
k�B
j�B
k�B
m�B
l�B
l�B
l�B
m�B
m�B
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
q�B
q�B
q�B
p�B
p�B
q�B
q�B
q�B
p�B
q�B
p�B
o�B
p�B
p�B
p�B
r�B
s�B
t�B
s�B
t�B
t�B
t�B
t�B
t�B
s�B
t�B
u�B
u�B
v�B
v�B
v�B
w�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.12(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901100033002019011000330020190110003300201901100200162019011002001620190110020016201901110024472019011100244720190111002447  JA  ARFMdecpA19c                                                                20190106093629  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190106003631  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190106003634  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190106003634  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190106003635  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190106003635  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190106003635  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190106003635  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190106003636  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190106003636                      G�O�G�O�G�O�                JA  ARUP                                                                        20190106005737                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190106153100  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20190109153300  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190109153300  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20190109170016  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190110152447  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131516                      G�O�G�O�G�O�                