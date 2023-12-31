CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-05-14T19:17:47Z AOML 3.0 creation; 2016-08-07T21:51:17Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150514191747  20160807145117  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               4A   AO  5287_9017_052                   2C  D   APEX                            6529                            072314                          846 @�P��p��1   @�P�W:�@0p ě���d��n��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    4A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C�C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�3D��fD�P D���D�� D�3D�FfD��3D��3D�	�D�0 D���D�ٚD�3D�I�Dڌ�D��3D�3D�<�D�l�D�Y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��R@θRA\)A'\)AG\)Ag\)A��A��A��A��AîAӮA�A�B�
B	�
B�
B�
B!�
B)�
B1�
B9�
BA�
BI�
BQ�
BY�
Ba�
Bi�
Bq�
By�
B��B��B��B��B��B��B��RB��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C u�Cu�Cu�Cu�Cu�C
u�Cu�Cu�Cu�Cu�C�]C�]Cu�Cu�Cu�Cu�C u�C"u�C$u�C&u�C(u�C*u�C,u�C.u�C0u�C2u�C4u�C6u�C8u�C:u�C<u�C>u�C@u�CBu�CDu�CFu�CHu�CJu�CLu�CNu�CPu�CRu�CTu�CVu�CXu�CZu�C\u�C^�]C`u�Cbu�Cd\)Cfu�Chu�Cju�Clu�Cnu�Cpu�Cru�Ctu�Cvu�Cxu�Czu�C|u�C~u�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�G�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�G�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#��D$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt�qDu
>Dy��D�D�^�D��RD�θD�!�D�UD���D���D�RD�>�D��RD��RD�!�D�XRDڛ�D���D��D�K�D�{�D�hR111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A�  A�  A�  A�  A�  A�  A���A���A���A���A�  A�  A�A�A�1A�1A�1A�1A�1A�1A�%A���A�ȴA��A̓A��A˝�A�bAɼjA��A�^5Aǧ�A�7LA��HA�~�A�=qA�A��/A�AÏ\A¬A���A�r�A���A��A�r�A���A��RA��HA�bA�  A��A��jA��hA�
=A��A�7LA�7LA���A�v�A�S�A���A���A��A��A�dZA�A�A�|�A�I�A�ZA�jA�XA���A�A�A�oA��`A��jA�t�A�;dA���A�r�A���A���A��A�~�A���A���A}��A|5?Az��Ax��At�DAr�uAq|�Ap�/Ao�Am�Ajz�Ae�A`Q�A\�+A[`BAY�AW��AUAS��AP�ANI�ALZAK��AG��AE�ADZAC
=A@5?A=�A:=qA7��A6�+A6A�A5��A5ƨA5t�A4ĜA2Q�A0��A0(�A/�A.��A.I�A,�\A*��A)%A($�A&^5A$��A$�A#VA"�DA!x�A bAz�AA�;A�AC�A�\AXA��AȴAI�AVAXA��A�Az�A�A�A �A��A
~�AhsA1A�A�A E�@�o@��R@�J@���@���@�E�@�G�@��@�/@�{@�^5@��@��j@���@���@��\@�x�@�\)@�=q@��#@�@�7L@��@�@��@�K�@�?}@���@�@�Z@�b@߾w@�33@�~�@�=q@�J@�X@���@���@۶F@��@�~�@�x�@��@׍P@��@�n�@�o@ָR@�p�@ԛ�@Ӆ@�O�@Ь@�r�@�I�@�Q�@�Ĝ@��`@��@��
@϶F@�K�@Ο�@�ff@�@���@�@͑h@�(�@��H@�5?@�@�@ɡ�@���@�  @��@��@�@š�@ģ�@��m@�C�@�v�@�E�@�V@¸R@���@�E�@�x�@��9@�Z@�b@���@��;@�|�@��@�ȴ@�{@�hs@�&�@�V@�V@��@��@���@�bN@�9X@�  @���@��@���@�S�@�"�@��y@��+@�M�@�@���@�G�@��@���@��@��@��/@��D@�9X@���@���@���@���@���@�-@�@�G�@���@��9@���@�z�@�(�@�  @���@�K�@���@��@���@�v�@�V@�-@���@�`B@��@��@���@�j@�  @���@��F@��@�l�@�K�@�+@��@�^5@���@���@�/@��@��j@��u@��@�bN@��@���@�K�@�o@�ȴ@���@�=q@��#@���@�O�@��@��`@��@� �@�1@���@��@��y@��H@���@��+@�$�@��#@�O�@���@��/@���@�A�@��@��F@�l�@�;d@��H@�ff@�M�@�J@��h@�7L@��`@��j@��u@�(�@���@��F@��@��@�-@��@���@�O�@���@��9@��u@�j@��@��m@��
@��@�K�@�+@��R@��+@�5?@�{@���@���@�p�@��@��@�z�@��@��;@�ƨ@���@�S�@�@��@�ȴ@��y@���@���@�n�@��#@���@���@�Ĝ@���@�j@��@���@��@�ȴ@�~�@�V@�=q@���@���@���@��h@�-@���@���@�p�@�z�@�Q�@�9X@�1@��;@���@�ƨ@���@���@�C�@��!@��@���@���@�%@�%@�7L@��/@�1@�ƨ@��@��@���@�v�@�^5@�V@�V@�@��T@�@�`B@�/@�&�@�\)@�A�@}�@v@k�@`Ĝ@W�@O;d@F��@?K�@6ff@/��@*�\@%�T@!%@o@�@�!@�@o@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�  A�  A�  A�  A�  A�  A�  A���A���A���A���A�  A�  A�A�A�1A�1A�1A�1A�1A�1A�%A���A�ȴA��A̓A��A˝�A�bAɼjA��A�^5Aǧ�A�7LA��HA�~�A�=qA�A��/A�AÏ\A¬A���A�r�A���A��A�r�A���A��RA��HA�bA�  A��A��jA��hA�
=A��A�7LA�7LA���A�v�A�S�A���A���A��A��A�dZA�A�A�|�A�I�A�ZA�jA�XA���A�A�A�oA��`A��jA�t�A�;dA���A�r�A���A���A��A�~�A���A���A}��A|5?Az��Ax��At�DAr�uAq|�Ap�/Ao�Am�Ajz�Ae�A`Q�A\�+A[`BAY�AW��AUAS��AP�ANI�ALZAK��AG��AE�ADZAC
=A@5?A=�A:=qA7��A6�+A6A�A5��A5ƨA5t�A4ĜA2Q�A0��A0(�A/�A.��A.I�A,�\A*��A)%A($�A&^5A$��A$�A#VA"�DA!x�A bAz�AA�;A�AC�A�\AXA��AȴAI�AVAXA��A�Az�A�A�A �A��A
~�AhsA1A�A�A E�@�o@��R@�J@���@���@�E�@�G�@��@�/@�{@�^5@��@��j@���@���@��\@�x�@�\)@�=q@��#@�@�7L@��@�@��@�K�@�?}@���@�@�Z@�b@߾w@�33@�~�@�=q@�J@�X@���@���@۶F@��@�~�@�x�@��@׍P@��@�n�@�o@ָR@�p�@ԛ�@Ӆ@�O�@Ь@�r�@�I�@�Q�@�Ĝ@��`@��@��
@϶F@�K�@Ο�@�ff@�@���@�@͑h@�(�@��H@�5?@�@�@ɡ�@���@�  @��@��@�@š�@ģ�@��m@�C�@�v�@�E�@�V@¸R@���@�E�@�x�@��9@�Z@�b@���@��;@�|�@��@�ȴ@�{@�hs@�&�@�V@�V@��@��@���@�bN@�9X@�  @���@��@���@�S�@�"�@��y@��+@�M�@�@���@�G�@��@���@��@��@��/@��D@�9X@���@���@���@���@���@�-@�@�G�@���@��9@���@�z�@�(�@�  @���@�K�@���@��@���@�v�@�V@�-@���@�`B@��@��@���@�j@�  @���@��F@��@�l�@�K�@�+@��@�^5@���@���@�/@��@��j@��u@��@�bN@��@���@�K�@�o@�ȴ@���@�=q@��#@���@�O�@��@��`@��@� �@�1@���@��@��y@��H@���@��+@�$�@��#@�O�@���@��/@���@�A�@��@��F@�l�@�;d@��H@�ff@�M�@�J@��h@�7L@��`@��j@��u@�(�@���@��F@��@��@�-@��@���@�O�@���@��9@��u@�j@��@��m@��
@��@�K�@�+@��R@��+@�5?@�{@���@���@�p�@��@��@�z�@��@��;@�ƨ@���@�S�@�@��@�ȴ@��y@���@���@�n�@��#@���@���@�Ĝ@���@�j@��@���@��@�ȴ@�~�@�V@�=q@���@���@���@��h@�-@���@���@�p�@�z�@�Q�@�9X@�1@��;@���@�ƨ@���@���@�C�@��!@��@���@���@�%@�%@�7L@��/@�1@�ƨ@��@��@���@�v�@�^5@�V@�V@�@��T@�@�`B@�/G�O�@�\)@�A�@}�@v@k�@`Ĝ@W�@O;d@F��@?K�@6ff@/��@*�\@%�T@!%@o@�@�!@�@o@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
W
B
VB
W
B
W
B
W
B
VB
W
B
W
B
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
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
XB
[#B
cTB
o�B+B�-B��BB,B5?BM�B_;Bp�Bt�B�B�1B�JB�\B�VB�B|�Bm�BW
B:^B#�B�B�B'�B;dBe`Bw�BA�BB�B�
B�LB�7BD�B)�B)�B�B�oB�B�7B��B�FB��B�Br�B[#BF�B49B!�B�B�BhBVBDBB  B
��B
�B
�NB
�FB
q�B
=qB
�B	�B	�
B	ɺB	��B	�LB	��B	��B	�PB	�7B	�B	w�B	cTB	E�B	)�B	�B	hB	
=B	B��B�B�ZB�5B�
B��BȴBĜBB��BÖBŢB��B��B�
B�B�B�#B�#B�B�/B�;B�BB�NB�NB�NB�B�B��B��B��B	B	B	DB	
=B	JB	JB	DB		7B	1B	+B	%B	B	B	B	B��B��B�B�B�sB�fB�`B�
B��B�)B��B�dB��B��B��B��B��B��B�B�-B�?B�jBŢB��B��B�BB�ZB�yB�sB�ZB�B��B��B��B��B��B��B��B��B�B�B�HB�BB�;B�ZB�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B	+B	�B	�B	�B	�B	 �B	�B	�B	�B	�B	'�B	49B	;dB	?}B	@�B	B�B	E�B	J�B	M�B	N�B	O�B	O�B	P�B	S�B	[#B	^5B	`BB	`BB	`BB	^5B	\)B	\)B	]/B	_;B	aHB	`BB	_;B	_;B	]/B	^5B	cTB	hsB	m�B	o�B	r�B	r�B	r�B	r�B	s�B	v�B	u�B	w�B	w�B	{�B	}�B	~�B	~�B	~�B	~�B	~�B	~�B	�B	�B	�B	�+B	�=B	�VB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�?B	�?B	�?B	�FB	�LB	�LB	�^B	�dB	�dB	�jB	�qB	�qB	�wB	�}B	��B	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�)B	�/B	�/B	�/B	�5B	�;B	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
  B
  B
B
B
  B
B
B
B
B
B
B
B
B
%B
+B
1B
JB
PB
JB
JB
DB
JB
PB
VB
VB
VB
VB
VB
bB
hB
hB
bB
PB
PB
VB
\B
bB
oB
bB
bB
bB
hB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
&�B
.B
33B
;dB
@�B
H�B
N�B
S�B
ZB
_;B
bNB
gmB
k�B
p�B
t�B
x�B
{�B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
V�B
U�B
V�B
V�B
V�B
U�B
V�B
V�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
W�B
[B
c7B
o�B	B�	B̪B�B+�B5BM�B_Bp~Bt�B��B�B�%B�7B�-B��B|�BmkBV�B:5B#�BzBB'�B;>Be9Bw�BAbB �B�UB��B�#B�BDrB)�B)�B��B�DB��B�B��B�B�hB��Br�BZ�BF�B4B!�BuBWB?B+BB�B
��B
��B
�mB
�#B
�B
q�B
=JB
�B	�B	��B	ɕB	�^B	�'B	��B	�\B	�-B	�B	��B	w�B	c1B	E�B	)�B	lB	HB	
B	 �B��B�wB�9B�B��B��BȘB�~B�pB�jB�uBŃBˤB��B��B��B��B� B�B��B�B�B�!B�+B�-B�+B�[B�tB��B��B��B	 �B	�B	B	
B	%B	#B	B		B	B	B	 B	�B	�B	�B	 �B��B��B�B�_B�MB�@B�:B��B��B�B��B�@B��B�vB�qB��B��B��B��B�B�B�DB�yBΰB��B�B�2B�QB�IB�0B�B��B��B��B��B��B��B��B��B�B�VB�B�B�B�1B�TB�\B�\B�hB�yB�B�B��B��B��B��B��B��B��B��B	 �B	�B	 B	SB	{B	{B	B	 �B	�B	�B	�B	�B	'�B	4
B	;5B	?PB	@UB	BbB	EvB	J�B	M�B	N�B	O�B	O�B	P�B	S�B	Z�B	^B	`B	`B	`B	^B	[�B	[�B	]B	_B	aB	`B	_B	_B	\�B	^B	c$B	hFB	mbB	oqB	r�B	r�B	rB	r�B	s�B	v�B	u�B	w�B	w�B	{�B	}�B	~�B	~�B	~�B	~�B	~�B	~�B	��B	��B	��B	��B	�B	�$B	�4B	�=B	�JB	�OB	�SB	�nB	�tB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�*B	�4B	�3B	�6B	�>B	�=B	�DB	�JB	�VB	�cB	�hB	�hB	�oB	�uB	�}B	�zB	�zB	�~B	ȀB	�B	ʎB	˒B	͠B	ΦB	вB	гB	ѺB	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�&B	�*B	�+B	�2B	�1B	�0B	�8B	�=B	�>B	�CB	�KB	�OB	�SB	�WB	�]B	�cB	�jB	�kB	�gB	�hB	�oB	�oB	�pB	�uB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
 �B	��B	��B
 �B
 �B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
B
B
B
B
B
B
 B
 B
 B
"B
,B
0B
3B
.B
B
B
B
'B
+B
8B
+B
*B
-B
1B
6B
?B
DB
EB
UB
]B
[B
\B
eB
kG�O�B
~B
"�B
&�B
-�B
2�B
;.B
@KB
H|B
N�B
S�B
Y�B
_B
bB
g5B
kOB
plB
t�B
x�B
{�B
�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.46 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451172016080714511720160807145117  AO  ARCAADJP                                                                    20150514191747    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150514191747  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150514191747  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145117  IP                  G�O�G�O�G�O�                