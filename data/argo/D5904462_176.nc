CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:56Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125956  20190405100800  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��)(d4T1   @��)�l+v@/��n���c�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D���D�9�D�vfD���D�� D�Y�D���D���D�	�D�S3D�� DǆfD�3D�C3DږfD�ٚD�3D�P D�l�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @W
>@��R@θRA\)A'\)AG\)Ag\)A��A��A��A��AîAӮA�A�B�
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
Bz=pB��B��B��B��RB��B��B��B��B��B��B��B��B��B��B��B��RB��B��BȸRB��B��B��B��B��B��B��B��B��B��B��B��B��C u�Cu�Cu�Cu�Cu�C
u�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�C u�C"u�C$u�C&u�C(u�C*u�C,u�C.u�C0u�C2u�C4u�C6u�C8u�C:u�C<u�C>u�C@u�CBu�CDu�CFu�CHu�CJu�CLu�CN�]CPu�CRu�CTu�CVu�CXu�CZu�C\u�C^u�C`u�Cbu�Cdu�Cfu�Chu�Cju�Clu�Cnu�Cpu�Cru�Ctu�Cvu�Cxu�Czu�C|�]C~u�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�D qD �qDqD�DqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt�qDy�>D��D�HRD��D�˅D���D�hRD��RD��D�RD�a�D���DǕD�!�D�Q�DڥD��RD�!�D�^�D�{�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�{A��A��mA�-A�ZA�dZA�n�A�t�A�z�AʃAʉ7AʋDAʋDAʋDAʍPAʏ\Aʕ�Aʙ�Aʝ�Aʝ�Aʟ�Aʡ�Aʡ�Aʡ�Aʙ�AʍPA�G�Aɇ+A�bNA�oA��A��AȋDAȶFA�bAǼjA��mA��A�t�A�dZA�;dA�7LA��/A�VA���AÓuA��/A��uA��^A�VA��A�-A�^5A�A�p�A�Q�A� �A�z�A�9XA���A��A��A��A���A��\A�n�A�A��9A�?}A�%A��`A��;A��;A�~�A�x�A�C�A�A�A�A��9A�^5A��A���A�%A�K�A�VA���A�bNA�\)A�A���A�1'A�1A��A��A��A�l�A�ZA��A�1'A|ĜA{33Az1Ax��At�`Ap�Ajz�AdQ�A_��AW�
AV�HAT��ARȴAP�AP1AN�RAJ~�AG�TAF�AD^5AB{A@1A<jA7��A6ffA4ȴA25?A0n�A/hsA.Q�A,��A,(�A,=qA)�7A'
=A&�uA'x�A&�yA&��A$�A#�
A#�A"�\A#�mA%|�A&I�A&JA%l�A$��A%�A$��A%"�A$��A$�HA#��A"�/A!C�AS�A�A�!A�9A9XA?}Ar�An�AA�A��AK�A�RA��A/AC�A=qA��AC�AVAz�AffAr�A��A�A�A��Az�A�A�wAO�A1'A��AO�An�A�mAx�A�A\)A
��A
�yA
��A	��A�AQ�Ax�A�\A5?A��A�-A��AK�A�A��A�AVAJA �A r�A (�@��@�o@���@���@��-@���@�7L@�(�@�S�@��@�v�@���@�/@�z�@� �@�t�@�@�J@�hs@�&�@�G�@��#@�@��@�7@�V@��;@�l�@�S�@�P@�b@�P@@��^@��@�33@��@��@��T@���@�Q�@��;@睲@�P@睲@���@�v�@�x�@���@�F@�(�@�ƨ@�P@�b@�9X@�@��#@��@��@އ+@���@ޟ�@ޗ�@�5?@��@�@ݑh@���@�Ĝ@�r�@ۍP@�33@�@�v�@ٲ-@�&�@׾w@֧�@�M�@թ�@�&�@�r�@���@Ӆ@Ӆ@�t�@�+@�"�@җ�@Ѳ-@�A�@�
=@���@�5?@��@�z�@�1'@˶F@���@�^5@��@��@ɩ�@ɡ�@ɡ�@ɉ7@�x�@�/@���@ț�@�j@��;@ǝ�@ǍP@�I�@�Ĝ@��/@ǅ@���@Ə\@Ə\@�~�@�5?@��#@�7L@Õ�@��@�V@�@�hs@��@��u@��@��u@�Z@��H@�=q@�J@��^@��-@��^@�-@�@���@��@�\)@�"�@��\@�5?@��T@�p�@�7L@��/@��D@�9X@���@��@�  @��
@���@�t�@��@�^5@���@�7L@��-@�M�@�$�@���@���@���@���@�t�@�S�@�C�@�+@�\)@��@��H@��@�O�@��@��/@��u@�bN@�9X@�  @���@���@�\)@�\)@�t�@�K�@�;d@��@��y@�^5@���@��@���@�x�@�O�@�7L@�r�@���@���@���@�;d@��!@�@�7L@��@��j@��@�I�@���@���@��@�\)@�o@�V@��@�&�@��@���@�b@�33@���@���@�hs@�G�@��@��/@�z�@��@��@���@�ƨ@��F@��@�33@��y@���@�V@�V@�^5@�$�@��7@���@���@���@�Z@�1@��;@���@�ƨ@���@��!@�-@��#@���@��@�X@�O�@��@��@���@��j@�j@�A�@��
@���@��@��@��@�ȴ@��F@�|�@~�@s��@iG�@`�u@X  @N�@F5?@<�/@5�T@-`B@)G�@$�D@ 1'@@�@=q@�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�{A��A��mA�-A�ZA�dZA�n�A�t�A�z�AʃAʉ7AʋDAʋDAʋDAʍPAʏ\Aʕ�Aʙ�Aʝ�Aʝ�Aʟ�Aʡ�Aʡ�Aʡ�Aʙ�AʍPA�G�Aɇ+A�bNA�oA��A��AȋDAȶFA�bAǼjA��mA��A�t�A�dZA�;dA�7LA��/A�VA���AÓuA��/A��uA��^A�VA��A�-A�^5A�A�p�A�Q�A� �A�z�A�9XA���A��A��A��A���A��\A�n�A�A��9A�?}A�%A��`A��;A��;A�~�A�x�A�C�A�A�A�A��9A�^5A��A���A�%A�K�A�VA���A�bNA�\)A�A���A�1'A�1A��A��A��A�l�A�ZA��A�1'A|ĜA{33Az1Ax��At�`Ap�Ajz�AdQ�A_��AW�
AV�HAT��ARȴAP�AP1AN�RAJ~�AG�TAF�AD^5AB{A@1A<jA7��A6ffA4ȴA25?A0n�A/hsA.Q�A,��A,(�A,=qA)�7A'
=A&�uA'x�A&�yA&��A$�A#�
A#�A"�\A#�mA%|�A&I�A&JA%l�A$��A%�A$��A%"�A$��A$�HA#��A"�/A!C�AS�A�A�!A�9A9XA?}Ar�An�AA�A��AK�A�RA��A/AC�A=qA��AC�AVAz�AffAr�A��A�A�A��Az�A�A�wAO�A1'A��AO�An�A�mAx�A�A\)A
��A
�yA
��A	��A�AQ�Ax�A�\A5?A��A�-A��AK�A�A��A�AVAJA �A r�A (�@��@�o@���@���@��-@���@�7L@�(�@�S�@��@�v�@���@�/@�z�@� �@�t�@�@�J@�hs@�&�@�G�@��#@�@��@�7@�V@��;@�l�@�S�@�P@�b@�P@@��^@��@�33@��@��@��T@���@�Q�@��;@睲@�P@睲@���@�v�@�x�@���@�F@�(�@�ƨ@�P@�b@�9X@�@��#@��@��@އ+@���@ޟ�@ޗ�@�5?@��@�@ݑh@���@�Ĝ@�r�@ۍP@�33@�@�v�@ٲ-@�&�@׾w@֧�@�M�@թ�@�&�@�r�@���@Ӆ@Ӆ@�t�@�+@�"�@җ�@Ѳ-@�A�@�
=@���@�5?@��@�z�@�1'@˶F@���@�^5@��@��@ɩ�@ɡ�@ɡ�@ɉ7@�x�@�/@���@ț�@�j@��;@ǝ�@ǍP@�I�@�Ĝ@��/@ǅ@���@Ə\@Ə\@�~�@�5?@��#@�7L@Õ�@��@�V@�@�hs@��@��u@��@��u@�Z@��H@�=q@�J@��^@��-@��^@�-@�@���@��@�\)@�"�@��\@�5?@��T@�p�@�7L@��/@��D@�9X@���@��@�  @��
@���@�t�@��@�^5@���@�7L@��-@�M�@�$�@���@���@���@���@�t�@�S�@�C�@�+@�\)@��@��H@��@�O�@��@��/@��u@�bN@�9X@�  @���@���@�\)@�\)@�t�@�K�@�;d@��@��y@�^5@���@��@���@�x�@�O�@�7L@�r�@���@���@���@�;d@��!@�@�7L@��@��j@��@�I�@���@���@��@�\)@�o@�V@��@�&�@��@���@�b@�33@���@���@�hs@�G�@��@��/@�z�@��@��@���@�ƨ@��F@��@�33@��y@���@�V@�V@�^5@�$�@��7@���@���@���@�Z@�1@��;@���@�ƨ@���@��!@�-@��#@���@��@�X@�O�@��@��@���@��j@�j@�A�@��
@���@��@��@��@�ȴ@��F@�|�@~�@s��@iG�@`�u@X  @N�@F5?@<�/@5�T@-`B@)G�@$�D@ 1'@@�@=q@�-111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	49B	+B	(�B	;dB	F�B	J�B	M�B	O�B	P�B	S�B	VB	W
B	W
B	W
B	W
B	W
B	XB	YB	[#B	\)B	]/B	]/B	]/B	_;B	aHB	ffB	q�B	�+B	�B	�XB	ǮB	�B
2-B
l�B
�PB
��BDBI�BZB|�B��B��B�'B�-B�B�B��B�#B�mB�B��B\B�B�B�B �B)�BB�BF�BF�BQ�BP�BO�BM�BJ�BB�B:^B7LB)�B�B�BDBBB��B�B�HB�5B�B�B��B�}B�FB�B��B{�BcTBT�B;dB{B
�B
��B
�-B
�DB
7LB
+B	�B	��B	B	�B	��B	��B	�uB	y�B	cTB	=qB	{B�B�B�B�B	PB	uB	VB	+B��B�B�B�sB�mB�ZB�HB�B��B��B��B��B��B�/B�mB�B�B�B��B	%B	 �B	(�B	7LB	+B	'�B	&�B	1'B	O�B	x�B	��B	�B	��B	��B	�LB	�jB	ĜB	ŢB	ĜB	�}B	�XB	�!B	��B	��B	�{B	��B	��B	�uB	�\B	��B	��B	�bB	�+B	�B	� B	x�B	�B	~�B	}�B	~�B	�B	�B	�{B	��B	��B	�'B	�3B	�9B	�9B	�9B	�-B	�B	��B	��B	��B	��B	��B	��B	�DB	�B	�B	�B	�B	|�B	v�B	r�B	k�B	dZB	cTB	gmB	l�B	l�B	k�B	k�B	m�B	m�B	jB	iyB	dZB	aHB	aHB	aHB	`BB	_;B	aHB	gmB	dZB	bNB	dZB	ffB	gmB	iyB	l�B	r�B	v�B	w�B	y�B	{�B	� B	�B	�B	�=B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�3B	�3B	�B	�B	�3B	�RB	�dB	�wB	ƨB	ɺB	ɺB	ÖB	ÖB	B	ĜB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�;B	�HB	�TB	�NB	�NB	�NB	�HB	�HB	�HB	�NB	�ZB	�ZB	�mB	�sB	�sB	�fB	�`B	�fB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B	��B
B
%B
B
B
B
  B
B
  B
  B
  B
B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
+B
+B
+B
+B
%B
%B
%B
%B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
	7B
1B
1B

=B

=B
DB
JB
JB

=B

=B

=B

=B

=B
JB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
VB
\B
VB
\B
\B
\B
bB
bB
bB
hB
hB
oB
uB
oB
bB
hB
hB
hB
hB
hB
oB
oB
uB
uB
�B
�B
�B
�B
�B
�B
�B
"�B
,B
1'B
7LB
<jB
B�B
F�B
K�B
Q�B
T�B
\)B
aHB
gmB
k�B
n�B
r�B
u�B
x�B
z�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	4B	*�B	(�B	;?B	F�B	J�B	M�B	O�B	P�B	S�B	U�B	V�B	V�B	V�B	V�B	V�B	W�B	X�B	Z�B	\B	]
B	]B	]
B	_B	a$B	f?B	q�B	�B	��B	�7B	ǇB	�eB
2B
lfB
�)B
��BBI�BY�B|�B�]B��B�B�B��B��B�[B��B�EB�qB��B4BZBxB�B �B)�BBfBF�BFBQ�BP�BO�BM�BJ�BBeB:5B7$B)�B�BmBB�B�B��B�aB�B�
B��B��B˛B�RB�B��B�gB{�Bc(BT�B;7BPB
�jB
��B
��B
�B
7B
�B	�dB	��B	�aB	��B	��B	�B	�FB	y�B	c$B	=AB	IB�~B�MB�NB�RB	B	DB	$B	�B��B�pB�YB�>B�:B�'B�B��B��BаB͟B̗BϨB��B�8B�hB�B�{B��B	�B	 �B	(�B	7B	*�B	'�B	&�B	0�B	O�B	x�B	��B	��B	��B	��B	�B	�4B	�gB	�kB	�eB	�FB	�!B	��B	��B	�vB	�GB	�]B	�WB	�>B	�&B	�cB	�`B	�+B	��B	��B	�B	x�B	��B	~�B	}�B	~�B	��B	��B	�EB	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�mB	�dB	�JB	�B	��B	��B	��B	��B	|�B	v�B	ryB	kNB	d"B	cB	g7B	lTB	lUB	kOB	kOB	mYB	mXB	jFB	i@B	d#B	aB	aB	aB	`B	_B	aB	g5B	d"B	bB	d"B	f/B	g3B	i@B	lRB	rwB	v�B	w�B	y�B	{�B	�B	��B	��B	�B	�6B	�YB	�`B	�nB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�(B	�=B	�oB	�B	�B	�[B	�ZB	�WB	�cB	ɀB	ˍB	ЬB	ϥB	ϦB	ЫB	ҺB	ҸB	ѲB	ѳB	ѰB	ѴB	ѲB	ӿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�2B	�8B	�7B	�,B	�%B	�*B	�:B	�8B	�>B	�<B	�EB	�DB	�JB	�]B	�ZB	�WB	�OB	�^B	�iB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�zB	��B	��B	��B	��B	�{B	�oB	�oB	�nB	�pB	�|B	��B	��B	��B	�yB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B	��B
 �B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B

B
B
B
B
	�B

B

B

 B

B
B
B
B
B
B
B
B
B
B
B
B
B
"B
B
!B
 B
B
'B
&B
'B
.B
+B
3B
8B
5B
%B
.B
,B
-B
-B
+B
2B
3B
9B
9B
EB
EB
IB
KB
KB
IB
WB
"�B
+�B
0�B
7B
</B
BSB
FkB
K�B
Q�B
T�B
[�B
a
B
g/B
kJB
nZB
rsB
u�B
x�B
z�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.46 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008002019040510080020190405100800  AO  ARCAADJP                                                                    20181121125956    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125956  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125956  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100800  IP                  G�O�G�O�G�O�                