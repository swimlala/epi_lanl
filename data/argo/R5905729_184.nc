CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-11T09:01:00Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         C   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    9   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    9    HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    9$   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    9(   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    98   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    9H   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    9X   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  9`   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  9�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  @  9�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        :    	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    :$   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    :(   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     :,   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    :L   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    :P   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     :T   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     :t   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     :�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    :�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_NB_SAMPLE_CTD_QC               	long_name         ,Global quality flag of NB_SAMPLE_CTD profile   conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        x  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kt   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  OT   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ^�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  b�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  r$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   NB_SAMPLE_CTD            
         	long_name         2Number of samples in each pressure bin for the CTD     
_FillValue        �     units         count      C_format      %5d    FORTRAN_format        I5     
resolution                �  �t   NB_SAMPLE_CTD_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  @  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �P   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �P   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �P   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  8  �P   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230511090100  20230511090100  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            NB_SAMPLE_CTD      �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @�*V����1   @�*Wo�I@*�\��N<�d��n.�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A       @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�ffB�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�(�@�(�A{A>{A^{A~{A�
=A�
=A�
=A�
=A�
=A�
=A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B�B�B�B���B�B�B�B�(�B�B��\B�B��\B�B�B�B�B�B�B�B�B�B�B�B�B�\B�B�B�B�B�B�B�C�HC�HC�HC�HC	�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/�HC1�HC3�HC5�HC7�HC9�HC;�HC=�HC?�HCA�HCC�HCE�HCG�HCI�HCK�HCM�HCO�HCQ�HCS�HCU�HCW�HCY�HC[�HC]�HC_�HCa�HCc�HCe�HCg�HCi�HCk�HCm�HCo�HCq�HCs�HCu�HCw�HCy�HC{�HC}�HC�HC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��qC��qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D xRD �RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD	xRD	�RD
xRD
�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RDxRD�RD xRD �RD!xRD!�RD"xRD"�RD#xRD#�RD$xRD$�RD%xRD%�RD&xRD&�RD'xRD'�RD(xRD(�RD)xRD)�RD*xRD*�RD+xRD+�RD,xRD,�RD-xRD-�RD.xRD.�RD/xRD/�RD0xRD0�RD1xRD1�RD2xRD2�RD3xRD3�RD4xRD4�RD5xRD5�RD6xRD6�RD7xRD7�RD8xRD8�RD9xRD9�RD:xRD:�RD;xRD;�RD<xRD<�RD=xRD=�RD>xRD>�RD?xRD?�RD@xRD@�RDAxRDA�RDBxRDB�RDCxRDC�RDDxRDD�RDExRDE�RDFxRDF�RDGxRDG�RDHxRDH�RDIxRDI�RDJxRDJ�RDKxRDK�RDLxRDL�RDMxRDM�RDNxRDN�RDOxRDO�RDPxRDP�RDQxRDQ�RDRxRDR�RDSxRDS�RDTxRDT�RDUxRDU�RDVxRDV�RDWxRDW�RDXxRDX�RDYxRDY�RDZxRDZ�RD[xRD[�RD\xRD\�RD]xRD]�RD^xRD^�RD_xRD_�RD`xRD`�RDaxRDa�RDbxRDb�RDcxRDc�RDdxRDd�RDexRDe�RDfxRDf�RDgxRDg�RDhxRDh�RDixRDi�RDjxRDj�RDkxRDk�RDlxRDl�RDmxRDm�RDnxRDn�RDoxRDo�RDpxRDp�RDqxRDq�RDrxRDr�RDsxRDs�RDtxRDt�RDuxRDu�RDvxRDv�RDwxRDw�RDxxRDx�RDyxRDy�RDzxRDz�RD{xRD{�RD|xRD|�RD}xRD}�RD~xRD~�RDxRD�RD�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�?\D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D¼)D��)D�<)D�|)Dü)D��)D�<)D�|)Dļ)D��)D�<)D�|)Dż)D��)D�<)D�|)DƼ)D��)D�<)D�|)DǼ)D��)D�<)D�|)Dȼ)D��)D�<)D�|)Dɼ)D��)D�<)D�|)Dʼ)D��)D�<)D�|)D˼)D��)D�<)D�|)D̼)D��)D�<)D�|)Dͼ)D��)D�<)D�|)Dμ)D��)D�<)D�|)Dϼ)D��)D�<)D�|)Dм)D��)D�<)D�|)DѼ)D��)D�<)D�|)DҼ)D��)D�<)D�|)DӼ)D��)D�<)D�|)DԼ)D��)D�<)D�|)Dռ)D��)D�<)D�|)Dּ)D��)D�<)D�|)D׼)D��)D�<)D�|)Dؼ)D��)D�<)D�|)Dټ)D��)D�<)D�|)Dڼ)D��)D�<)D�|)Dۼ)D��)D�<)D�|)Dܼ)D��)D�<)D�|)Dݼ)D��)D�<)D�|)D޼)D��)D�<)D�|)D߼)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D�)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��)D�<)D�|)D��)D��\D�?\D�\D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�%A��A��A��A��A�{A��A��A��A��A��A��A��A��A��A��A��A�&�A�$�A�&�A�-A�1'A�/A�+A�(�A�bA���A��A��A��`AہA�bNA���A���Aڰ!A�M�A�S�A��mA�VA�`BA���AȸRAǇ+A���A� �AċDA��
A�A��HA�5?A���A�+A��uA�=qA���A�33A��A�/A���A��HA��;A���A��DA��A���A�K�A��A�hsA��A�dZA���A�K�A���A�1'A�G�A���A���A�9XA�Q�A}��Aw�-At5?Apn�AoG�Amp�Ak��Ag��AdĜAb9XA^�9A[�TAY�#AW�wAV�AUdZAS&�APz�ALv�AJv�AF~�ABffA@�A?�A>��A> �A=�PA<�A<bA8�9A6�A4jA2ȴA1�wA1dZA0�!A-A,�DA+dZA(��A'�TA';dA& �A%VA$jA$bA$1A#XA"��A"9XA!%A�hA��A1'A�A%A�RA�uAv�AE�A5?A �A  A��A+AĜAA�A�A�A�A`BAG�A"�A�yAĜAQ�A�A��A+AȴAZA$�A�;A��Ax�A"�A�yA��A=qAJA��A�A��A{A|�AG�A33A��A�jAZA1A��AC�A�A��AȴA�+AZA9XA�mA33A%A
�/A
�9A
�DA
v�A
�A	�;A	�
A	��A	�-A	��A	��A	|�A	O�A	C�A	7LA	�A�`A�/A��A��A�AM�A�A1A�A��Ap�AXAA�9A�A$�A�hAp�AS�A��A�jA�+AQ�A�A��Al�A�RAE�A�mA�PA"�A ��A =qA  �A 1@���@�l�@�/@�|�@��@�&�@� �@��!@�v�@�E�@��-@�9X@�@�@�{@���@��
@��H@�E�@�O�@�b@�S�@�!@��@�@�x�@�hs@��@�@�(�@�w@�+@�n�@�{@噚@���@��m@��@�@�dZ@�+@�hs@���@���@�ƨ@�\)@��@�v�@���@�p�@�j@���@�@���@�v�@��@١�@ف@��@ش9@�j@�9X@� �@׮@�~�@�5?@��@պ^@Չ7@�O�@�?}@�7L@��@���@�r�@�ƨ@��@�@с@�G�@���@Ь@�b@�t�@�C�@�+@��@Ώ\@�n�@��#@�&�@̃@�I�@�(�@��@�1@���@�ƨ@˥�@˕�@�l�@�\)@�\)@�S�@�K�@�o@ʟ�@�x�@���@ȃ@�I�@��@�  @��m@�ƨ@�S�@��y@Ƨ�@Ƈ+@�v�@�V@�E�@�J@ź^@�hs@�7L@��/@öF@�o@�n�@��h@���@���@�@�E�@��@��D@��@��@��#@�`B@��@��D@�I�@���@�;d@��H@��-@�G�@�7L@��j@�9X@�(�@� �@��@��;@��P@�;d@�E�@��@���@���@��9@���@�j@�I�@�1@��F@�\)@���@�v�@�G�@��D@�b@���@�S�@�"�@��@��R@��\@�ff@�=q@��#@���@��7@�x�@�X@�/@���@�r�@�\)@��@��!@���@��h@��@�j@��;@���@���@���@�K�@��@��@��y@���@���@���@��+@�n�@�=q@�J@��#@�O�@�V@���@��@��@�K�@�o@���@�=q@�@���@�x�@�hs@�`B@�X@�X@�X@�X@���@���@�Q�@��@���@�t�@�K�@�;d@�"�@���@���@�~�@�n�@�ff@�E�@��@���@�O�@���@���@�r�@�A�@��m@���@��@�
=@��R@��+@�=q@��T@��^@�X@��@��@� �@��;@��P@�"�@�o@�ȴ@�ff@��@��#@���@�O�@�/@�%@���@�I�@��;@��F@��P@�@��!@��\@��+@�n�@�{@���@��@�hs@�`B@�&�@�%@��`@���@���@���@�b@��@���@�|�@�l�@���@��!@���@��\@��+@�M�@��@��@���@���@� �@���@��@�K�@��+@��^@��7@�`B@�7L@��@��@���@�Ĝ@��9@��@�A�@��@�dZ@�;d@��@�o@��y@���@��\@�~�@�ff@�V@�=q@�@��T@���@���@��7@��@�w@��@��@�P@\)@�@~�y@~�R@~��@~�+@~E�@~$�@~{@}��@}/@|��@|Z@{��@{t�@zn�@yx�@x��@xr�@xbN@x1'@xb@w|�@w�@v��@u��@t�@t�/@t�/@t��@t�j@t��@tz�@tZ@tI�@tI�@t�@s�m@r��@rn�@rM�@q��@q��@p��@p�9@p��@p�@p �@o�;@o�@o;d@m�@m/@l��@lZ@l1@k�m@k�F@k��@kS�@k"�@k@j�\@i�@ihs@i&�@hr�@h1'@hb@g�@g�P@fv�@eO�@d�/@dj@c��@c�
@c�@cS�@c33@c33@c"�@co@b�@b��@b=q@a��@a��@a��@`Q�@_�@_��@_��@_K�@_
=@^�R@^ff@]@]/@\�@\��@\(�@[t�@[o@Z��@Z��@Z~�@Z^5@Z-@Y�@Y��@Y7L@XĜ@X��@XbN@X �@W��@W\)@W+@W
=@V�@VV@U��@U�@Up�@U/@T��@T9X@S�F@S�@St�@St�@SdZ@S33@R�H@Q��@P  @O�@O�P@O|�@O;d@N��@N��@N�y@N�y@N�R@Nv�@N{@M��@M��@Mp�@M�@L��@L9X@K�
@K��@K��@KS�@K"�@K@J�@J��@J�!@J~�@I�#@I%@H��@HbN@HA�@H  @G��@G��@Gl�@G�@F�y@Fȴ@Fff@F{@E�T@EO�@D�D@D(�@D1@CdZ@C"�@B�!@B-@A��@@Ĝ@@A�@?�;@?��@?+@>��@>E�@>$�@>{@=��@=�-@=`B@=V@<��@<�@<9X@;�
@;�F@;��@;�@;dZ@;33@;@:��@:�\@:~�@:^5@:^5@:=q@:�@9�@9x�@8��@8r�@8b@7��@7;d@6�R@6��@6��@6�+@6$�@5?}@4��@4�/@4�/@4�/@4�/@4�j@4z�@4I�@3��@3t�@3C�@2�@2�!@2~�@2^5@2M�@2M�@2=q@2=q@2-@2J@1��@1��@1�^@1�7@1G�@1�@1%@0��@0�@0r�@0  @/\)@.�@.ȴ@.ȴ@.��@.��@.�+@.�+@.ff@-@,�@,�D@,z�@,j@,Z@,I�@,1@+�F@+C�@+o@*�@*�@*��@*�\@*^5@*�@)�@)��@)�7@)hs@)�@(Ĝ@(�9@(�@(A�@(b@'�@'�w@'�P@'\)@'�@&ff@%��@$��@$�@$z�@$Z@$Z@$I�@$9X@$�@$�@$1@#ƨ@#��@#��@#t�@#dZ@#S�@#S�@#C�@#33@#@"��@"~�@!�@!��@ �`@ �@ r�@ r�@ 1'@��@�@�y@�@��@v�@ff@ff@E�@5?@�@�h@O�@�@V@�@j@�@��@��@�m@ƨ@��@t�@"�@�@��@^5@J@��@hs@�@��@�@Q�@b@�@��@��@��@�w@l�@
=@��@ȴ@v�@$�@��@`B@��@��@��@z�@Z@Z@Z@Z@(�@�m@��@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�%A��A��A��A��A�{A��A��A��A��A��A��A��A��A��A��A��A�&�A�$�A�&�A�-A�1'A�/A�+A�(�A�bA���A��A��A��`AہA�bNA���A���Aڰ!A�M�A�S�A��mA�VA�`BA���AȸRAǇ+A���A� �AċDA��
A�A��HA�5?A���A�+A��uA�=qA���A�33A��A�/A���A��HA��;A���A��DA��A���A�K�A��A�hsA��A�dZA���A�K�A���A�1'A�G�A���A���A�9XA�Q�A}��Aw�-At5?Apn�AoG�Amp�Ak��Ag��AdĜAb9XA^�9A[�TAY�#AW�wAV�AUdZAS&�APz�ALv�AJv�AF~�ABffA@�A?�A>��A> �A=�PA<�A<bA8�9A6�A4jA2ȴA1�wA1dZA0�!A-A,�DA+dZA(��A'�TA';dA& �A%VA$jA$bA$1A#XA"��A"9XA!%A�hA��A1'A�A%A�RA�uAv�AE�A5?A �A  A��A+AĜAA�A�A�A�A`BAG�A"�A�yAĜAQ�A�A��A+AȴAZA$�A�;A��Ax�A"�A�yA��A=qAJA��A�A��A{A|�AG�A33A��A�jAZA1A��AC�A�A��AȴA�+AZA9XA�mA33A%A
�/A
�9A
�DA
v�A
�A	�;A	�
A	��A	�-A	��A	��A	|�A	O�A	C�A	7LA	�A�`A�/A��A��A�AM�A�A1A�A��Ap�AXAA�9A�A$�A�hAp�AS�A��A�jA�+AQ�A�A��Al�A�RAE�A�mA�PA"�A ��A =qA  �A 1@���@�l�@�/@�|�@��@�&�@� �@��!@�v�@�E�@��-@�9X@�@�@�{@���@��
@��H@�E�@�O�@�b@�S�@�!@��@�@�x�@�hs@��@�@�(�@�w@�+@�n�@�{@噚@���@��m@��@�@�dZ@�+@�hs@���@���@�ƨ@�\)@��@�v�@���@�p�@�j@���@�@���@�v�@��@١�@ف@��@ش9@�j@�9X@� �@׮@�~�@�5?@��@պ^@Չ7@�O�@�?}@�7L@��@���@�r�@�ƨ@��@�@с@�G�@���@Ь@�b@�t�@�C�@�+@��@Ώ\@�n�@��#@�&�@̃@�I�@�(�@��@�1@���@�ƨ@˥�@˕�@�l�@�\)@�\)@�S�@�K�@�o@ʟ�@�x�@���@ȃ@�I�@��@�  @��m@�ƨ@�S�@��y@Ƨ�@Ƈ+@�v�@�V@�E�@�J@ź^@�hs@�7L@��/@öF@�o@�n�@��h@���@���@�@�E�@��@��D@��@��@��#@�`B@��@��D@�I�@���@�;d@��H@��-@�G�@�7L@��j@�9X@�(�@� �@��@��;@��P@�;d@�E�@��@���@���@��9@���@�j@�I�@�1@��F@�\)@���@�v�@�G�@��D@�b@���@�S�@�"�@��@��R@��\@�ff@�=q@��#@���@��7@�x�@�X@�/@���@�r�@�\)@��@��!@���@��h@��@�j@��;@���@���@���@�K�@��@��@��y@���@���@���@��+@�n�@�=q@�J@��#@�O�@�V@���@��@��@�K�@�o@���@�=q@�@���@�x�@�hs@�`B@�X@�X@�X@�X@���@���@�Q�@��@���@�t�@�K�@�;d@�"�@���@���@�~�@�n�@�ff@�E�@��@���@�O�@���@���@�r�@�A�@��m@���@��@�
=@��R@��+@�=q@��T@��^@�X@��@��@� �@��;@��P@�"�@�o@�ȴ@�ff@��@��#@���@�O�@�/@�%@���@�I�@��;@��F@��P@�@��!@��\@��+@�n�@�{@���@��@�hs@�`B@�&�@�%@��`@���@���@���@�b@��@���@�|�@�l�@���@��!@���@��\@��+@�M�@��@��@���@���@� �@���@��@�K�@��+@��^@��7@�`B@�7L@��@��@���@�Ĝ@��9@��@�A�@��@�dZ@�;d@��@�o@��y@���@��\@�~�@�ff@�V@�=q@�@��T@���@���@��7@��@�w@��@��@�P@\)@�@~�y@~�R@~��@~�+@~E�@~$�@~{@}��@}/@|��@|Z@{��@{t�@zn�@yx�@x��@xr�@xbN@x1'@xb@w|�@w�@v��@u��@t�@t�/@t�/@t��@t�j@t��@tz�@tZ@tI�@tI�@t�@s�m@r��@rn�@rM�@q��@q��@p��@p�9@p��@p�@p �@o�;@o�@o;d@m�@m/@l��@lZ@l1@k�m@k�F@k��@kS�@k"�@k@j�\@i�@ihs@i&�@hr�@h1'@hb@g�@g�P@fv�@eO�@d�/@dj@c��@c�
@c�@cS�@c33@c33@c"�@co@b�@b��@b=q@a��@a��@a��@`Q�@_�@_��@_��@_K�@_
=@^�R@^ff@]@]/@\�@\��@\(�@[t�@[o@Z��@Z��@Z~�@Z^5@Z-@Y�@Y��@Y7L@XĜ@X��@XbN@X �@W��@W\)@W+@W
=@V�@VV@U��@U�@Up�@U/@T��@T9X@S�F@S�@St�@St�@SdZ@S33@R�H@Q��@P  @O�@O�P@O|�@O;d@N��@N��@N�y@N�y@N�R@Nv�@N{@M��@M��@Mp�@M�@L��@L9X@K�
@K��@K��@KS�@K"�@K@J�@J��@J�!@J~�@I�#@I%@H��@HbN@HA�@H  @G��@G��@Gl�@G�@F�y@Fȴ@Fff@F{@E�T@EO�@D�D@D(�@D1@CdZ@C"�@B�!@B-@A��@@Ĝ@@A�@?�;@?��@?+@>��@>E�@>$�@>{@=��@=�-@=`B@=V@<��@<�@<9X@;�
@;�F@;��@;�@;dZ@;33@;@:��@:�\@:~�@:^5@:^5@:=q@:�@9�@9x�@8��@8r�@8b@7��@7;d@6�R@6��@6��@6�+@6$�@5?}@4��@4�/@4�/@4�/@4�/@4�j@4z�@4I�@3��@3t�@3C�@2�@2�!@2~�@2^5@2M�@2M�@2=q@2=q@2-@2J@1��@1��@1�^@1�7@1G�@1�@1%@0��@0�@0r�@0  @/\)@.�@.ȴ@.ȴ@.��@.��@.�+@.�+@.ff@-@,�@,�D@,z�@,j@,Z@,I�@,1@+�F@+C�@+o@*�@*�@*��@*�\@*^5@*�@)�@)��@)�7@)hs@)�@(Ĝ@(�9@(�@(A�@(b@'�@'�w@'�P@'\)@'�@&ff@%��@$��@$�@$z�@$Z@$Z@$I�@$9X@$�@$�@$1@#ƨ@#��@#��@#t�@#dZ@#S�@#S�@#C�@#33@#@"��@"~�@!�@!��@ �`@ �@ r�@ r�@ 1'@��@�@�y@�@��@v�@ff@ff@E�@5?@�@�h@O�@�@V@�@j@�@��@��@�m@ƨ@��@t�@"�@�@��@^5@J@��@hs@�@��@�@Q�@b@�@��@��@��@�w@l�@
=@��@ȴ@v�@$�@��@`B@��@��@��@z�@Z@Z@Z@Z@(�@�m@��@S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
0!B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
/B
0!B
0!B
0!B
/B
/B
/B
/B
/B
.B
.B
,B
-B
0!B
33B
33B
0!B
:^B
7LB
J�B
J�B
C�B
"�B
R�B
p�B
m�B
dZB
@�B
��B
�XB
��B
=B�B�B\)B�RB�yB�mB!�B#�B1'B�B�mB��BȴB�DBD�BZBZBG�B7LB0!B33B6FB.B�B
�B
�?B
k�B
�B
s�B
M�B	��B	�B
DB	�B	�-B	B	��B	�wB	�B	��B	r�B	n�B	_;B	I�B	D�B	E�B	F�B	F�B	?}B	-B	!�B	bB	"�B	oB	�B	A�B	cTB	cTB	ffB	iyB	dZB	aHB	K�B	bNB	l�B	�B	�bB	��B	�uB	|�B	�oB	��B	��B	�RB	ŢB	ĜB	��B	�)B	�`B	�yB	�TB	�`B	�B	�yB	�B	��B
PB
VB
�B
�B
�B
 �B
 �B
#�B
"�B
 �B
�B
�B
!�B
$�B
+B
.B
1'B
2-B
33B
2-B
0!B
/B
)�B
"�B
%�B
9XB
<jB
=qB
A�B
A�B
B�B
C�B
B�B
D�B
C�B
B�B
E�B
C�B
C�B
D�B
D�B
E�B
J�B
L�B
J�B
J�B
I�B
I�B
I�B
J�B
M�B
M�B
M�B
L�B
L�B
L�B
I�B
E�B
L�B
M�B
M�B
L�B
L�B
J�B
K�B
O�B
N�B
N�B
N�B
N�B
M�B
L�B
N�B
N�B
L�B
L�B
M�B
M�B
K�B
L�B
J�B
J�B
K�B
J�B
G�B
G�B
H�B
E�B
C�B
D�B
A�B
>wB
F�B
F�B
C�B
C�B
D�B
D�B
B�B
?}B
<jB
8RB
=qB
=qB
<jB
;dB
:^B
;dB
?}B
>wB
;dB
5?B
+B
)�B
,B
/B
.B
)�B
1'B
/B
+B
$�B
$�B
)�B
(�B
#�B
$�B
$�B
&�B
#�B
!�B
#�B
#�B
$�B
+B
-B
,B
)�B
'�B
&�B
'�B
%�B
#�B
$�B
!�B
�B
�B
!�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
hB
PB
hB
oB
{B
{B
�B
{B
uB
oB
hB
uB
{B
{B
{B
{B
uB
oB
hB
hB
\B

=B
PB
PB
JB
\B
PB
VB
\B
VB
hB
{B
VB
PB
{B
�B
{B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
!�B
"�B
"�B
#�B
#�B
"�B
"�B
!�B
"�B
#�B
#�B
"�B
!�B
 �B
�B
�B
!�B
�B
�B
!�B
"�B
!�B
#�B
'�B
'�B
&�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
&�B
&�B
%�B
$�B
&�B
&�B
&�B
$�B
#�B
'�B
&�B
&�B
(�B
(�B
+B
,B
,B
,B
,B
,B
+B
(�B
(�B
(�B
)�B
+B
,B
-B
-B
-B
,B
,B
.B
.B
.B
-B
,B
+B
)�B
,B
-B
.B
.B
-B
/B
-B
-B
.B
/B
/B
.B
0!B
.B
/B
.B
/B
1'B
1'B
0!B
2-B
1'B
0!B
2-B
2-B
33B
33B
5?B
6FB
5?B
33B
49B
7LB
6FB
5?B
6FB
8RB
9XB
8RB
6FB
7LB
7LB
9XB
<jB
;dB
;dB
<jB
<jB
<jB
:^B
7LB
9XB
=qB
=qB
<jB
:^B
<jB
>wB
?}B
>wB
=qB
=qB
=qB
;dB
9XB
8RB
>wB
<jB
<jB
;dB
<jB
C�B
D�B
E�B
E�B
E�B
F�B
F�B
E�B
D�B
D�B
D�B
C�B
H�B
H�B
I�B
H�B
G�B
I�B
I�B
I�B
I�B
I�B
H�B
I�B
I�B
H�B
F�B
D�B
C�B
L�B
M�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
L�B
L�B
M�B
M�B
L�B
K�B
M�B
O�B
P�B
Q�B
Q�B
Q�B
O�B
P�B
O�B
O�B
P�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
R�B
Q�B
R�B
T�B
S�B
S�B
S�B
VB
VB
VB
T�B
VB
T�B
S�B
Q�B
S�B
VB
W
B
W
B
XB
XB
XB
W
B
W
B
W
B
VB
VB
VB
XB
W
B
YB
ZB
YB
W
B
T�B
T�B
YB
ZB
ZB
\)B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
ZB
[#B
[#B
[#B
XB
\)B
^5B
]/B
]/B
]/B
]/B
\)B
\)B
\)B
^5B
^5B
^5B
^5B
_;B
aHB
aHB
aHB
aHB
aHB
aHB
aHB
`BB
aHB
bNB
bNB
aHB
aHB
bNB
cTB
cTB
bNB
aHB
bNB
cTB
dZB
cTB
bNB
dZB
cTB
e`B
ffB
ffB
e`B
dZB
cTB
`BB
_;B
ffB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
hsB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
ffB
gmB
hsB
iyB
hsB
iyB
iyB
iyB
iyB
iyB
hsB
gmB
gmB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
k�B
k�B
k�B
jB
jB
l�B
l�B
k�B
l�B
l�B
l�B
k�B
l�B
m�B
n�B
n�B
n�B
n�B
p�B
q�B
q�B
p�B
q�B
p�B
p�B
q�B
q�B
q�B
q�B
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
s�B
s�B
r�B
r�B
r�B
s�B
s�B
t�B
u�B
w�B
v�B
v�B
u�B
s�B
w�B
x�B
x�B
x�B
x�B
w�B
w�B
w�B
v�B
v�B
w�B
x�B
x�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
y�B
z�B
z�B
{�B
z�B
z�B
z�B
y�B
x�B
{�B
|�B
}�B
}�B
}�B
}�B
|�B
{�B
y�B
y�B
~�B
~�B
~�B
~�B
~�B
}�B
}�B
}�B
� B
� B
� B
� B
�B
� B
� B
� B
� B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
~�B
� B
� B
�B
�B
�B
�%B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�B
�B
�B
�B
�B
�B
�%B
�1B
�1B
�+B
�B
�%B
�+B
�1B
�1B
�1B
�=B
�7B
�7B
�7B
�1B
�1B
�=B
�=B
�=B
�=B
�7B
�DB
�JB
�PB
�PB
�PB
�PB
�PB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�\B
�\B
�\B
�bB
�bB
�hB
�bB
�bB
�\B
�\B
�bB
�bB
�\B
�\B
�\B
�\B
�bB
�hB
�hB
�oB
�oB
�uB
�uB
�oB
�oB
�hB
�oB
�oB
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
0!B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
/B
0!B
0!B
0!B
/B
/B
/B
/B
/B
.B
.B
,B
-B
0!B
33B
33B
0!B
:^B
7LB
J�B
J�B
C�B
"�B
R�B
p�B
m�B
dZB
@�B
��B
�XB
��B
=B�B�B\)B�RB�yB�mB!�B#�B1'B�B�mB��BȴB�DBD�BZBZBG�B7LB0!B33B6FB.B�B
�B
�?B
k�B
�B
s�B
M�B	��B	�B
DB	�B	�-B	B	��B	�wB	�B	��B	r�B	n�B	_;B	I�B	D�B	E�B	F�B	F�B	?}B	-B	!�B	bB	"�B	oB	�B	A�B	cTB	cTB	ffB	iyB	dZB	aHB	K�B	bNB	l�B	�B	�bB	��B	�uB	|�B	�oB	��B	��B	�RB	ŢB	ĜB	��B	�)B	�`B	�yB	�TB	�`B	�B	�yB	�B	��B
PB
VB
�B
�B
�B
 �B
 �B
#�B
"�B
 �B
�B
�B
!�B
$�B
+B
.B
1'B
2-B
33B
2-B
0!B
/B
)�B
"�B
%�B
9XB
<jB
=qB
A�B
A�B
B�B
C�B
B�B
D�B
C�B
B�B
E�B
C�B
C�B
D�B
D�B
E�B
J�B
L�B
J�B
J�B
I�B
I�B
I�B
J�B
M�B
M�B
M�B
L�B
L�B
L�B
I�B
E�B
L�B
M�B
M�B
L�B
L�B
J�B
K�B
O�B
N�B
N�B
N�B
N�B
M�B
L�B
N�B
N�B
L�B
L�B
M�B
M�B
K�B
L�B
J�B
J�B
K�B
J�B
G�B
G�B
H�B
E�B
C�B
D�B
A�B
>wB
F�B
F�B
C�B
C�B
D�B
D�B
B�B
?}B
<jB
8RB
=qB
=qB
<jB
;dB
:^B
;dB
?}B
>wB
;dB
5?B
+B
)�B
,B
/B
.B
)�B
1'B
/B
+B
$�B
$�B
)�B
(�B
#�B
$�B
$�B
&�B
#�B
!�B
#�B
#�B
$�B
+B
-B
,B
)�B
'�B
&�B
'�B
%�B
#�B
$�B
!�B
�B
�B
!�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
hB
PB
hB
oB
{B
{B
�B
{B
uB
oB
hB
uB
{B
{B
{B
{B
uB
oB
hB
hB
\B

=B
PB
PB
JB
\B
PB
VB
\B
VB
hB
{B
VB
PB
{B
�B
{B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
 �B
 �B
!�B
"�B
"�B
#�B
#�B
"�B
"�B
!�B
"�B
#�B
#�B
"�B
!�B
 �B
�B
�B
!�B
�B
�B
!�B
"�B
!�B
#�B
'�B
'�B
&�B
%�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
&�B
&�B
%�B
$�B
&�B
&�B
&�B
$�B
#�B
'�B
&�B
&�B
(�B
(�B
+B
,B
,B
,B
,B
,B
+B
(�B
(�B
(�B
)�B
+B
,B
-B
-B
-B
,B
,B
.B
.B
.B
-B
,B
+B
)�B
,B
-B
.B
.B
-B
/B
-B
-B
.B
/B
/B
.B
0!B
.B
/B
.B
/B
1'B
1'B
0!B
2-B
1'B
0!B
2-B
2-B
33B
33B
5?B
6FB
5?B
33B
49B
7LB
6FB
5?B
6FB
8RB
9XB
8RB
6FB
7LB
7LB
9XB
<jB
;dB
;dB
<jB
<jB
<jB
:^B
7LB
9XB
=qB
=qB
<jB
:^B
<jB
>wB
?}B
>wB
=qB
=qB
=qB
;dB
9XB
8RB
>wB
<jB
<jB
;dB
<jB
C�B
D�B
E�B
E�B
E�B
F�B
F�B
E�B
D�B
D�B
D�B
C�B
H�B
H�B
I�B
H�B
G�B
I�B
I�B
I�B
I�B
I�B
H�B
I�B
I�B
H�B
F�B
D�B
C�B
L�B
M�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
L�B
L�B
L�B
M�B
M�B
L�B
K�B
M�B
O�B
P�B
Q�B
Q�B
Q�B
O�B
P�B
O�B
O�B
P�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
S�B
R�B
Q�B
R�B
T�B
S�B
S�B
S�B
VB
VB
VB
T�B
VB
T�B
S�B
Q�B
S�B
VB
W
B
W
B
XB
XB
XB
W
B
W
B
W
B
VB
VB
VB
XB
W
B
YB
ZB
YB
W
B
T�B
T�B
YB
ZB
ZB
\)B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
ZB
[#B
[#B
[#B
XB
\)B
^5B
]/B
]/B
]/B
]/B
\)B
\)B
\)B
^5B
^5B
^5B
^5B
_;B
aHB
aHB
aHB
aHB
aHB
aHB
aHB
`BB
aHB
bNB
bNB
aHB
aHB
bNB
cTB
cTB
bNB
aHB
bNB
cTB
dZB
cTB
bNB
dZB
cTB
e`B
ffB
ffB
e`B
dZB
cTB
`BB
_;B
ffB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
hsB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
ffB
gmB
hsB
iyB
hsB
iyB
iyB
iyB
iyB
iyB
hsB
gmB
gmB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
k�B
k�B
k�B
jB
jB
l�B
l�B
k�B
l�B
l�B
l�B
k�B
l�B
m�B
n�B
n�B
n�B
n�B
p�B
q�B
q�B
p�B
q�B
p�B
p�B
q�B
q�B
q�B
q�B
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
s�B
s�B
r�B
r�B
r�B
s�B
s�B
t�B
u�B
w�B
v�B
v�B
u�B
s�B
w�B
x�B
x�B
x�B
x�B
w�B
w�B
w�B
v�B
v�B
w�B
x�B
x�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
y�B
z�B
z�B
{�B
z�B
z�B
z�B
y�B
x�B
{�B
|�B
}�B
}�B
}�B
}�B
|�B
{�B
y�B
y�B
~�B
~�B
~�B
~�B
~�B
}�B
}�B
}�B
� B
� B
� B
� B
�B
� B
� B
� B
� B
�B
�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
~�B
� B
� B
�B
�B
�B
�%B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�B
�B
�B
�B
�B
�B
�%B
�1B
�1B
�+B
�B
�%B
�+B
�1B
�1B
�1B
�=B
�7B
�7B
�7B
�1B
�1B
�=B
�=B
�=B
�=B
�7B
�DB
�JB
�PB
�PB
�PB
�PB
�PB
�JB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�\B
�\B
�\B
�bB
�bB
�hB
�bB
�bB
�\B
�\B
�bB
�bB
�\B
�\B
�\B
�\B
�bB
�hB
�hB
�oB
�oB
�uB
�uB
�oB
�oB
�hB
�oB
�oB
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�              ! ! #                  ! ( . ' % A x > . !   ' !       ! ( / %                             %                               !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            , B000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000  PRES            TEMP            PSAL            NB_SAMPLE_CTD   PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.12 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230511090100                                          AO  ARCAADJP                                                                    20230511090100    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230511090100  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230511090100  QCF$                G�O�G�O�G�O�0               