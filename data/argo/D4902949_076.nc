CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-10-27T09:00:37Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
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
resolution        =���   axis      Z        
�  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Dh   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     
�  G(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  R    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     
�  T�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
�  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  j�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
�  m�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
�  {H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
�  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
�  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     
�  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��        ��Argo profile    3.1 1.2 19500101000000  20191027090037  20200924132254  4902949 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               LA   AO  6976                            2C  D   NAVIS_A                         0823                            170210                          863 @��U!�S1   @��U���@3�p��
=�c���Q�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      LA   A   A   @�33@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_y�D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�fD�9�D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @~�R@�(�@�(�A{A?�A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]��D^xRD^�RD_q�D_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D���D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�\D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��\D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�\D��)D��D�5�D�|)D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��
A���A���A��A��#A��A��#A��/A��#A���A�ƨA�ƨA�Aܴ9Aܡ�Aܕ�A܁A� �A���AۑhA�r�A�n�A�^5A�I�A��A�Aڏ\A���A���A���Aٟ�AكA�S�A�/A�&�A�$�A�oA�
=A���A��A��`A��Aؗ�A׶FA�A�v�A�;dAʣ�A��A�v�A��AȮAȝ�Aȉ7Aȡ�AȺ^A�?}A�1A�?}A��^A�1'A��A�G�A��A��^A�dZA��A�t�A���A��A�ffA��A��FA�9XA���A���A���A��A�33A���A��A�"�A��A�G�A��`A�`BA�-A��yA�A�&�A���A�oA�bNA�jA�ƨA���A�^5A�ƨA��`A��^A~ffA}|�Aw�7AuG�Ap^5Am�mAlA�AkVAiAg��Afv�Ad�jAc��Ab�HAb�A`��A_ƨA^��A]��A]/A\M�AZ�uAVv�AUx�AT�HAR1APĜAN~�AKhsAJz�AI`BAGp�AE��AC�TA@�A>1A<$�A9�^A8bA6�A5/A2r�A2JA0��A-��A+;dA(�A&��A#�
A!`BAƨAhsAC�AS�AȴA{A��A��AQ�A��A�HA|�A�9A�A�AƨA��AhsA"�AbNA�A"�AbNAA
=A	�hA	33A�!Ar�A��A��A`BA;dA��A^5A|�AjA5?A�A��A��AjAQ�A1'A�A��A �u@�=q@�`B@��m@�+@���@�$�@��^@�hs@��u@��@�"�@��H@�5?@��@��@�@��@��@�@�9@�1'@�S�@��@�@���@�F@��y@�5?@�=q@�E�@��@�o@�M�@��@��@���@�@���@�-@�x�@�?}@�@��H@���@�1@��@��/@�(�@�;d@��H@���@�9@�M�@�A�@�|�@�v�@ڸR@��y@�ff@�v�@���@ڏ\@ٲ-@���@�ȴ@ٙ�@�M�@�n�@�@أ�@���@�dZ@��@�V@�t�@�|�@ԋD@�@�5?@�x�@���@���@�?}@�`B@�I�@�@�o@ϥ�@�j@�o@̃@�\)@�E�@�5?@ȼj@�?}@�`B@ɉ7@��@�t�@϶F@�b@�&�@�j@���@�I�@��@�"�@��@�K�@��@��m@�@ļj@�&�@��
@�C�@��@�o@��y@���@�n�@�-@�@��@�5?@�5?@�=q@�5?@�x�@��F@��D@�?}@���@��@���@�$�@���@��@�ƨ@���@���@�ff@��@��^@���@�X@���@���@��F@���@�V@�=q@��#@�^5@���@���@�=q@���@�hs@���@�z�@�(�@�A�@�1'@��u@��/@�Ĝ@���@�/@���@�
=@�@�Ĝ@��@��@�"�@�K�@���@��@�{@�^5@�t�@��@���@�?}@�z�@�o@�33@�;d@�-@���@��/@��u@�ȴ@��^@�A�@���@�dZ@�K�@�33@�+@�o@��H@�^5@��7@�?}@��`@��@�z�@�z�@�j@�bN@�9X@�(�@�  @��;@��
@��F@���@��@�|�@�|�@�dZ@�\)@�S�@�K�@�K�@�\)@�l�@��!@��@��-@�`B@�/@���@��u@�j@�A�@�A�@�9X@�1'@�(�@�b@��@��
@�ƨ@��w@��w@��F@���@�|�@�C�@�"�@��@���@��!@���@��+@�E�@��-@�?}@��@���@��/@��D@��;@���@�|�@�S�@�K�@�"�@��@�@��H@�ȴ@�~�@�J@���@���@��7@��@�X@�G�@�?}@�&�@���@�I�@�b@�b@��m@��F@��@�dZ@�\)@�33@���@�n�@�-@��T@�O�@���@���@�j@�b@�|�@���@�ȴ@���@�v�@�5?@��@�hs@��@���@��D@�bN@�A�@�1@��w@�|�@�+@��+@�$�@��#@���@�`B@�V@���@���@�I�@� �@��;@��P@�dZ@�33@�ff@�J@�@�hs@��9@�Q�@�9X@�ƨ@�dZ@�+@�@��@�V@�-@�{@��7@���@���@�r�@�A�@��@�b@�  @�w@��@|�@K�@+@
=@~�@~��@~�+@~v�@~v�@~�+@~v�@~V@}�T@}@}�@}�@|�/@|�@|1@{t�@{33@{o@z�@z��@zn�@zM�@z�@y��@x��@x�@xb@w�w@w�@w��@w�P@wK�@v��@v�y@vff@v@u�@u�-@u�@up�@u�@t9X@s�
@s�F@s�@sS�@s"�@s@r�!@rM�@q��@q�^@qG�@p��@p��@p��@p�9@p1'@o��@oK�@n�@n�@n�+@nV@m�T@mp�@l�@lZ@k��@kS�@k33@k"�@j��@i��@i��@i7L@h��@h�@h �@g��@fȴ@f�R@f�+@fE�@f$�@e�@d��@d(�@c�
@c"�@b�@`��@`�9@`��@`�u@`��@`��@_�@]p�@\9X@["�@Z�@Z�\@ZJ@Y�@XbN@X �@W�@W�P@W;d@V�@V�R@V5?@U�h@U�-@U`B@SdZ@Q�7@P �@O�;@P �@O�@O�@P  @O;d@O
=@M/@L�/@L�/@L�/@L��@L�@L�@M�@L�@MV@L�/@N5?@PbN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��
A���A���A��A��#A��A��#A��/A��#A���A�ƨA�ƨA�Aܴ9Aܡ�Aܕ�A܁A� �A���AۑhA�r�A�n�A�^5A�I�A��A�Aڏ\A���A���A���Aٟ�AكA�S�A�/A�&�A�$�A�oA�
=A���A��A��`A��Aؗ�A׶FA�A�v�A�;dAʣ�A��A�v�A��AȮAȝ�Aȉ7Aȡ�AȺ^A�?}A�1A�?}A��^A�1'A��A�G�A��A��^A�dZA��A�t�A���A��A�ffA��A��FA�9XA���A���A���A��A�33A���A��A�"�A��A�G�A��`A�`BA�-A��yA�A�&�A���A�oA�bNA�jA�ƨA���A�^5A�ƨA��`A��^A~ffA}|�Aw�7AuG�Ap^5Am�mAlA�AkVAiAg��Afv�Ad�jAc��Ab�HAb�A`��A_ƨA^��A]��A]/A\M�AZ�uAVv�AUx�AT�HAR1APĜAN~�AKhsAJz�AI`BAGp�AE��AC�TA@�A>1A<$�A9�^A8bA6�A5/A2r�A2JA0��A-��A+;dA(�A&��A#�
A!`BAƨAhsAC�AS�AȴA{A��A��AQ�A��A�HA|�A�9A�A�AƨA��AhsA"�AbNA�A"�AbNAA
=A	�hA	33A�!Ar�A��A��A`BA;dA��A^5A|�AjA5?A�A��A��AjAQ�A1'A�A��A �u@�=q@�`B@��m@�+@���@�$�@��^@�hs@��u@��@�"�@��H@�5?@��@��@�@��@��@�@�9@�1'@�S�@��@�@���@�F@��y@�5?@�=q@�E�@��@�o@�M�@��@��@���@�@���@�-@�x�@�?}@�@��H@���@�1@��@��/@�(�@�;d@��H@���@�9@�M�@�A�@�|�@�v�@ڸR@��y@�ff@�v�@���@ڏ\@ٲ-@���@�ȴ@ٙ�@�M�@�n�@�@أ�@���@�dZ@��@�V@�t�@�|�@ԋD@�@�5?@�x�@���@���@�?}@�`B@�I�@�@�o@ϥ�@�j@�o@̃@�\)@�E�@�5?@ȼj@�?}@�`B@ɉ7@��@�t�@϶F@�b@�&�@�j@���@�I�@��@�"�@��@�K�@��@��m@�@ļj@�&�@��
@�C�@��@�o@��y@���@�n�@�-@�@��@�5?@�5?@�=q@�5?@�x�@��F@��D@�?}@���@��@���@�$�@���@��@�ƨ@���@���@�ff@��@��^@���@�X@���@���@��F@���@�V@�=q@��#@�^5@���@���@�=q@���@�hs@���@�z�@�(�@�A�@�1'@��u@��/@�Ĝ@���@�/@���@�
=@�@�Ĝ@��@��@�"�@�K�@���@��@�{@�^5@�t�@��@���@�?}@�z�@�o@�33@�;d@�-@���@��/@��u@�ȴ@��^@�A�@���@�dZ@�K�@�33@�+@�o@��H@�^5@��7@�?}@��`@��@�z�@�z�@�j@�bN@�9X@�(�@�  @��;@��
@��F@���@��@�|�@�|�@�dZ@�\)@�S�@�K�@�K�@�\)@�l�@��!@��@��-@�`B@�/@���@��u@�j@�A�@�A�@�9X@�1'@�(�@�b@��@��
@�ƨ@��w@��w@��F@���@�|�@�C�@�"�@��@���@��!@���@��+@�E�@��-@�?}@��@���@��/@��D@��;@���@�|�@�S�@�K�@�"�@��@�@��H@�ȴ@�~�@�J@���@���@��7@��@�X@�G�@�?}@�&�@���@�I�@�b@�b@��m@��F@��@�dZ@�\)@�33@���@�n�@�-@��T@�O�@���@���@�j@�b@�|�@���@�ȴ@���@�v�@�5?@��@�hs@��@���@��D@�bN@�A�@�1@��w@�|�@�+@��+@�$�@��#@���@�`B@�V@���@���@�I�@� �@��;@��P@�dZ@�33@�ff@�J@�@�hs@��9@�Q�@�9X@�ƨ@�dZ@�+@�@��@�V@�-@�{@��7@���@���@�r�@�A�@��@�b@�  @�w@��@|�@K�@+@
=@~�@~��@~�+@~v�@~v�@~�+@~v�@~V@}�T@}@}�@}�@|�/@|�@|1@{t�@{33@{o@z�@z��@zn�@zM�@z�@y��@x��@x�@xb@w�w@w�@w��@w�P@wK�@v��@v�y@vff@v@u�@u�-@u�@up�@u�@t9X@s�
@s�F@s�@sS�@s"�@s@r�!@rM�@q��@q�^@qG�@p��@p��@p��@p�9@p1'@o��@oK�@n�@n�@n�+@nV@m�T@mp�@l�@lZ@k��@kS�@k33@k"�@j��@i��@i��@i7L@h��@h�@h �@g��@fȴ@f�R@f�+@fE�@f$�@e�@d��@d(�@c�
@c"�@b�@`��@`�9@`��@`�u@`��@`��@_�@]p�@\9X@["�@Z�@Z�\@ZJ@Y�@XbN@X �@W�@W�P@W;d@V�@V�R@V5?@U�h@U�-@U`B@SdZ@Q�7@P �@O�;@P �@O�@O�@P  @O;d@O
=@M/@L�/@L�/@L�/@L��@L�@L�@M�@L�@MV@L�/@N5?@PbN111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
H�B
H�B
H�B
H�B
H�B
G�B
G�B
F�B
G�B
E�B
D�B
B�B
@�B
@�B
?}B
?}B
<jB
=qB
B�B
H�B
G�B
G�B
G�B
F�B
H�B
H�B
H�B
H�B
G�B
G�B
F�B
F�B
E�B
C�B
K�B
Q�B
H�B
\)B
YB
�B
�BXB��B�B��B��B�B�B�TB��B�LB�?B��B�/B%B�B0!B/B+B%�BB�BN�BYBB�B2-B'�B �B�BPB��B��B�B�NB�5B�B��BȴBÖB�9B��Bv�BjBVBK�B1'BB
��B
�7B
�B
z�B
gmB
<jB
oB
DB	�#B	ƨB	��B	�7B	�B	y�B	x�B	n�B	l�B	hsB	cTB	_;B	]/B	XB	R�B	O�B	G�B	E�B	>wB	<jB	/B	$�B	"�B	�B	�B	bB	+B	  B��B��B�B�B�HB�B��BƨB��B�jB�XB�-B�B�B��B��B�uB�PB�bB�PB�+B�%B�B�7B�B�B�B� B~�B}�B{�By�Bw�Bw�Bw�Bw�Bw�Bv�Bv�Bw�B�B�B�B�B�B�%B�%B�B�B�B�B�+B�+B�+B�1B�=B�PB�\B�bB�oB��B��B��B��B��B��B��B��B��B��B�B�9B�RB�^B�dB�qB�wB�}B�wB��B��B�}B��B��B��B�}B�wB�qBÖBŢBĜBŢBƨBƨBƨBǮBǮBǮBƨBǮBǮBȴBɺB��B��B��B��B��B�
B�
B�B�fB	PB	+B	B	B	  B��B	  B��B��B��B�B��B��B��B	  B	1B	JB	DB	VB	�B	#�B	(�B	)�B	)�B	$�B	!�B	�B	%�B	"�B	 �B	&�B	49B	8RB	6FB	6FB	;dB	?}B	>wB	B�B	?}B	:^B	=qB	A�B	J�B	E�B	@�B	=qB	=qB	G�B	M�B	R�B	T�B	W
B	dZB	}�B	�+B	�PB	��B	��B	�DB	�B	y�B	r�B	w�B	x�B	}�B	�B	y�B	t�B	jB	r�B	v�B	w�B	w�B	x�B	x�B	y�B	y�B	{�B	~�B	�B	�B	�B	�B	�1B	�=B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�?B	�'B	�B	�3B	�?B	�LB	�RB	�RB	�LB	�LB	�!B	�B	�'B	�9B	�^B	ÖB	ƨB	ɺB	��B	��B	��B	ǮB	ÖB	ÖB	B	ĜB	ĜB	��B	ɺB	ƨB	ɺB	��B	�B	�/B	�BB	�HB	�)B	�BB	�NB	�HB	�;B	�NB	�TB	�5B	�B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�)B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�/B	�5B	�5B	�5B	�;B	�BB	�NB	�ZB	�fB	�`B	�`B	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
	7B

=B
DB
DB
DB
JB
JB
JB
PB
PB
PB
PB
VB
VB
\B
bB
bB
bB
hB
oB
oB
oB
uB
{B
{B
{B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
!�B
!�B
!�B
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
$�B
$�B
$�B
$�B
$�B
%�B
$�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
-B
-B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
2-B
33B
33B
33B
33B
33B
33B
5?B
6FB
7LB
7LB
7LB
8RB
8RB
9XB
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
=qB
?}B
@�B
@�B
?}B
@�B
@�B
@�B
@�B
@�B
C�B
B�B
C�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
B�B
@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
H�B
IB
H�B
H�B
H�B
IB
HB
G�B
GB
I:B
G8B
E~B
CB
@�B
@�B
?�B
@iB
<�B
?�B
EHB
ITB
HB
HLB
H6B
G�B
INB
H�B
H�B
H�B
G�B
G�B
F�B
F�B
E�B
DaB
NMB
V�B
RtB
j�B
`NB
��B
��BU�B�'B�XB��B�xB��B��B�;BĄB�?B��B��B�B�B�B1�B0�B5-B0�BG2BZ0Bj�BG,B3�B+�B('B�B}B��B��B��B�B�BۈB�jB��B�+B�/B��By�BoPBX�BR�BF�B�B
��B
��B
��B
��B
w�B
C�B
�B
�B	�B	ИB	��B	��B	��B	|�B	}]B	qB	pgB	j�B	e}B	a&B	`�B	ZGB	U�B	R�B	H�B	HB	CxB	G&B	1�B	&�B	*�B	HB	�B	�B		hB	&B�B�B��B�B�;B�B��BɬB��B�B��B�-B��B�]B��B�1B��B��B��B��B�5B��B��B�mB��B�[B��B��B��B�B>B{�ByABxBBxKBxJBx\Bw�Bx�B�B��B��B��B��B�B�?B��B��B�nB�"B��B��B��B�B��B�IB��B��B�NB��B�B�B�#B�B�AB��B��B�!B��B��B��B�	B�B��B��B�>B��B��B��B��B��B�B®B��B��B�ZB�MB��BŞB�[B�oBǋB��BǠBƮBǻB�;BɯBǎB�-BǽB��B��B��B��B�,B�KB��B�B�5B�hB�B	3B	9B	rB	�B	mB��B	�B	#B�B�VB�bB�B��B��B�`B	�B	�B	B	B	WB	"�B	(�B	+#B	+�B	&!B	"aB	B	*`B	%B	 �B	%�B	6nB	9yB	7IB	5�B	;�B	@FB	>fB	DB	A*B	:UB	<�B	@�B	LB	HB	A�B	>~B	=�B	I)B	M_B	R�B	T�B	TGB	`AB	}�B	��B	�B	��B	��B	�`B	�rB	|�B	r�B	w�B	xB	~1B	�B	{�B	zB	l~B	s�B	wB	w�B	x#B	ySB	y5B	zSB	z'B	|B	~�B	�'B	�!B	�LB	�sB	��B	��B	�QB	�jB	�vB	��B	��B	��B	�vB	��B	�9B	� B	��B	�gB	��B	�%B	�|B	��B	�WB	�@B	�B	�QB	��B	��B	�aB	��B	�>B	��B	��B	�jB	��B	��B	��B	�B	�GB	��B	�B	��B	�lB	ɀB	�?B	єB	�RB	�qB	�B	�lB	õB	�tB	�OB	ˆB	�B	�IB	�XB	�B	�B	ܾB	�^B	�)B	�B	�TB	��B	��B	�mB	��B	��B	��B	�?B	�yB	��B	�,B	�0B	�B	�0B	�gB	�B	�mB	ؠB	ضB	ًB	�vB	�4B	�WB	�QB	݄B	�XB	܁B	�sB	�MB	�vB	�lB	�iB	�JB	�GB	�iB	�SB	�TB	�RB	�HB	�5B	�`B	��B	��B	��B	�	B	��B	�!B	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	�B	�B	��B	�#B	�B	�rB	��B	��B	��B	�DB	��B	�*B	��B	��B	��B	��B	��B	��B	��B	��B	�:B	�}B	�7B	�B	�B	��B	�B	��B	��B	�B	��B	�XB	�4B	��B	�"B	�$B	�&B	�B	��B	�#B	�bB	�cB	�BB	�TB	��B	�tB	�@B	��B
 �B
 �B
�B
_B
\B
QB
~B
�B
�B
�B
�B
�B
rB
iB
~B
�B
�B
�B
	�B

�B
�B
�B
�B
�B
wB
�B
�B
�B
�B
�B
�B
�B
jB
�B
�B
�B
jB
	B
�B
%B
B
�B
�B
�B
PB
�B
�B
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
�B
�B
[B
CB
�B
�B
�B
�B
!B
�B
�B
<B
 vB
 9B
!BB
!!B
 �B
 �B
 �B
!B
"!B
!�B
"XB
#3B
"�B
#B
#B
"�B
#6B
#�B
$:B
$B
$B
%B
%B
%B
%5B
%GB
&9B
%)B
&WB
&9B
&B
%�B
&B
&eB
'oB
';B
(`B
(B
(DB
('B
(UB
)XB
)[B
*dB
*GB
*nB
+'B
+B
+PB
+�B
,KB
,`B
,AB
,_B
-[B
-pB
.�B
..B
.CB
/TB
/DB
/�B
/�B
0�B
0jB
0�B
1�B
2�B
3qB
3SB
3UB
36B
3NB
4lB
6�B
7CB
8&B
7�B
7�B
8�B
9 B
9�B
9�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
;]B
;�B
=B
>�B
@�B
@�B
@hB
?�B
@�B
@�B
@�B
@�B
A�B
C�B
B�B
C�B
B�B
B�B
B�B
B{B
B�B
B�B
B�B
C�B
B3B
@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<03�<zg{<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<6&
<<��<#�
<G�<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<5$�<#�
<#�
<#�
<#�
<���<�]<#�
<#�
<#�
<#�
<�X�<#�
<#�
<Vd�<#�
<2��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<>IX<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.12 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  202009211514302020092115143020200921151430  AO  ARCAADJP                                                                    20191027090037    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20191027090037  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20191027090037  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20200921151430  QC  PRES            @�33D�� G�O�                PM  ARSQCTM V1.1                                                                20200921151430  QC  PSAL            @�33D�� G�O�                PM  ARSQCOWGV1.1CTD_2018v2 + Argo_2018v01                                       20200924132254  IP                  G�O�G�O�G�O�                