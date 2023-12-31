CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-08T19:17:29Z AOML 3.0 creation; 2016-08-07T21:51:15Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150308191729  20160807145115  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               'A   AO  5287_9017_039                   2C  D   APEX                            6529                            072314                          846 @�?��=�1   @�?�8�@1�������d���vȴ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    'A   B   B   @@  @�  @�  A��A!��AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BPffBXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"�C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dys3D�	�D�I�D�s3D��3D�3D�I�D��3D��fD��3D�33D���D��fD��fD�&fDچfD��D���D�L�D�|�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @]p�@��R@θRA��A(��AH��Ag\)A��A��A��A��AîAӮA�A�B�
B	�
B�
Bp�B!�
B)�
B1�
B9�
BA�
BI�
BR=pBZ=pBa�
Bi�
Bq�
By�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C u�Cu�Cu�Cu�Cu�C
u�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�C �]C"�]C$u�C&u�C(u�C*u�C,u�C.u�C0u�C2u�C4u�C6u�C8u�C:u�C<u�C>u�C@u�CBu�CDu�CFu�CHu�CJu�CLu�CN\)CPu�CRu�CTu�CVu�CXu�CZu�C\u�C^u�C`u�Cbu�Cdu�Cfu�Chu�Cju�Clu�Cnu�Cpu�Cru�Ctu�Cvu�Cxu�Czu�C|u�C~u�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�G�C�:�C�.C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt��Dy��D�RD�XRD���D���D�!�D�XRD���D��D��D�A�D��RD��D�D�5DڕD�˅D��D�[�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A���A��A���A���A���A���A��A��A��A��A��A��A��A��A��A���A�  A�A�
=A�1A�
=A�JA�bA��A��A͸RA�5?A˶FA�1A�{A���AɑhA�^5AȋDAǙ�A��AƋDAŇ+A��A�^5Aš�A�Aũ�Aŏ\A�9XA�v�A��A���Aå�A�ZA�ƨA���A��hA�bNA�E�A���A���A�hsA��+A�bA�+A��wA��-A�^5A�ZA��RA��9A�A�M�A��9A���A��/A���A��PA�t�A�z�A�hsA�\)A��A��-A�ƨA�bNA�A�A��-A��A���A���A��FA�hsA�;dA�-A�=qA���A�  A�A��A�5?A���A��A�VA�G�A�
=A��A��RA���A~��Ax��Au�TAt��ArjAoS�Am�mAk/Ah�AcXA_�wAZ�jAS7LAM/AJ1AG�hAC\)A@��A@-A<�yA9dZA8��A7x�A5A3�A3?}A2��A2v�A1��A0�+A/XA/%A/A.��A-��A,�jA+�A*ffA*jA(v�A%�A$�A%;dA%7LA#�A#��A#\)A �A n�A;dA�\AVA�AhsA�TA33A�\AM�A��A��A��A\)A$�A	|�AffAXA~�A��A~�A�-A�HAffA�FA �@�J@���@�p�@��D@��H@���@���@���@�p�@�A�@�w@�P@�;d@�~�@���@��@�E�@���@���@�(�@�@��@���@��@�Q�@�"�@�-@���@�@�@�l�@��@�!@�$�@���@�r�@ޏ\@�-@���@ݑh@���@ە�@���@�V@�{@��#@٩�@ف@�p�@�?}@�z�@��@�(�@��y@�=q@�X@��/@ЋD@�1'@��;@��m@Ͼw@���@ЋD@�X@ѩ�@�bN@��#@͡�@ͩ�@ͩ�@�hs@�O�@�z�@�"�@�o@�G�@��@�l�@�o@�{@�p�@Ĭ@�j@�Z@�Ĝ@��@���@��#@��@�j@�  @ģ�@�7L@���@���@�Ĝ@�Q�@��@�V@�hs@��m@�1@��@�1'@Õ�@��@�b@�j@�I�@�dZ@�33@�;d@�C�@�33@+@�{@��-@��/@���@��h@�z�@�I�@��@�\)@��@�o@�=q@��h@��@��@�b@���@���@��+@��h@�V@�|�@�E�@���@�~�@�$�@��-@��@�X@�/@�%@���@���@��D@�Q�@��
@�S�@��@�ȴ@���@�M�@��@�hs@���@�r�@���@���@�t�@�;d@��H@���@��+@��@���@�X@�V@��j@�z�@�r�@�bN@�r�@�j@�Z@�I�@��@�  @���@��m@��w@��P@�S�@�33@�
=@�n�@�$�@���@�7L@�V@��@� �@�  @�  @�  @���@�
=@��R@�v�@�M�@�-@��#@��@�hs@��@��D@�(�@���@��@��@��@��`@��j@��u@�j@�I�@�b@�l�@�o@���@���@���@��+@�J@�O�@�7L@�V@���@���@��9@�bN@��@��@��P@�K�@�;d@�
=@�ȴ@�=q@��@�`B@��/@��@��D@�r�@�Q�@�  @��F@��@��P@�S�@�C�@�33@��y@���@���@���@��\@��+@�ff@�5?@���@��7@�`B@�?}@��@�Q�@��m@���@�|�@�\)@�C�@�+@�o@�
=@�@�@���@���@��@��R@��+@�M�@��@��^@�O�@�V@��@���@�z�@� �@���@�ƨ@��P@�S�@��@��y@��R@�=q@�@��#@��h@�1@���@|j@t��@l�@d�@YG�@Ol�@Hb@?�@7;d@2=q@-?}@'�;@$��@!x�@�@�^@�/@r�@�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A��A��A���A��A���A���A���A���A��A��A��A��A��A��A��A��A��A���A�  A�A�
=A�1A�
=A�JA�bA��A��A͸RA�5?A˶FA�1A�{A���AɑhA�^5AȋDAǙ�A��AƋDAŇ+A��A�^5Aš�A�Aũ�Aŏ\A�9XA�v�A��A���Aå�A�ZA�ƨA���A��hA�bNA�E�A���A���A�hsA��+A�bA�+A��wA��-A�^5A�ZA��RA��9A�A�M�A��9A���A��/A���A��PA�t�A�z�A�hsA�\)A��A��-A�ƨA�bNA�A�A��-A��A���A���A��FA�hsA�;dA�-A�=qA���A�  A�A��A�5?A���A��A�VA�G�A�
=A��A��RA���A~��Ax��Au�TAt��ArjAoS�Am�mAk/Ah�AcXA_�wAZ�jAS7LAM/AJ1AG�hAC\)A@��A@-A<�yA9dZA8��A7x�A5A3�A3?}A2��A2v�A1��A0�+A/XA/%A/A.��A-��A,�jA+�A*ffA*jA(v�A%�A$�A%;dA%7LA#�A#��A#\)A �A n�A;dA�\AVA�AhsA�TA33A�\AM�A��A��A��A\)A$�A	|�AffAXA~�A��A~�A�-A�HAffA�FA �@�J@���@�p�@��D@��H@���@���@���@�p�@�A�@�w@�P@�;d@�~�@���@��@�E�@���@���@�(�@�@��@���@��@�Q�@�"�@�-@���@�@�@�l�@��@�!@�$�@���@�r�@ޏ\@�-@���@ݑh@���@ە�@���@�V@�{@��#@٩�@ف@�p�@�?}@�z�@��@�(�@��y@�=q@�X@��/@ЋD@�1'@��;@��m@Ͼw@���@ЋD@�X@ѩ�@�bN@��#@͡�@ͩ�@ͩ�@�hs@�O�@�z�@�"�@�o@�G�@��@�l�@�o@�{@�p�@Ĭ@�j@�Z@�Ĝ@��@���@��#@��@�j@�  @ģ�@�7L@���@���@�Ĝ@�Q�@��@�V@�hs@��m@�1@��@�1'@Õ�@��@�b@�j@�I�@�dZ@�33@�;d@�C�@�33@+@�{@��-@��/@���@��h@�z�@�I�@��@�\)@��@�o@�=q@��h@��@��@�b@���@���@��+@��h@�V@�|�@�E�@���@�~�@�$�@��-@��@�X@�/@�%@���@���@��D@�Q�@��
@�S�@��@�ȴ@���@�M�@��@�hs@���@�r�@���@���@�t�@�;d@��H@���@��+@��@���@�X@�V@��j@�z�@�r�@�bN@�r�@�j@�Z@�I�@��@�  @���@��m@��w@��P@�S�@�33@�
=@�n�@�$�@���@�7L@�V@��@� �@�  @�  @�  @���@�
=@��R@�v�@�M�@�-@��#@��@�hs@��@��D@�(�@���@��@��@��@��`@��j@��u@�j@�I�@�b@�l�@�o@���@���@���@��+@�J@�O�@�7L@�V@���@���@��9@�bN@��@��@��P@�K�@�;d@�
=@�ȴ@�=q@��@�`B@��/@��@��D@�r�@�Q�@�  @��F@��@��P@�S�@�C�@�33@��y@���@���@���@��\@��+@�ff@�5?@���@��7@�`B@�?}@��@�Q�@��m@���@�|�@�\)@�C�@�+@�o@�
=@�@�@���@���@��@��R@��+@�M�@��@��^@�O�@�V@��@���@�z�@� �@���@�ƨ@��P@�S�@��@��y@��R@�=q@�@��#G�O�@�1@���@|j@t��@l�@d�@YG�@Ol�@Hb@?�@7;d@2=q@-?}@'�;@$��@!x�@�@�^@�/@r�@�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�sB
�sB
�sB
�sB
�sB
�sB
�sB
�sB
�sB
�mB
�mB
�mB
�mB
�sB
�mB
�sB
�sB
�sB
�sB
�sB
�mB
�mB
�mB
�yB
�B
�B
��BB5?B�^B�BB�BcTB�DB�uB��B�}B�B��B��B��BB�
B�BBhB�B�B�B�B1'B0!B%�B��B�LB��B��B�HB/B"�B�BB�+B~�BYB;dB+B�B�B�BB�dB��B�7Bz�B�B�'B��B:^BK�BS�BhsB�Bx�BZB=qBVBPBB��B�B�`B�}B�B��B��B{�BB�B�BbBB
�B
�ZB
�B
��B
ƨB
�qB
��B
�{B
v�B
gmB
N�B
%�B
PB	��B	��B	�BB	��B	�^B	��B	� B	q�B	T�B	1'B	�B	DB��B�B�B�`B�#B��B��BɺB��B�qB�qB�}B��BBŢBǮB��B��B��B��BɺBÖBȴB��B��BÖBɺB�
B�#B�BB�/B�B��B��B��B��B�wB�}B�qB�LB�!B�B��B��B��B�oB�\B�7B�B|�Bz�By�By�By�Bz�Bz�Bz�By�B{�B|�B{�B{�Bz�Bz�By�By�By�By�Bz�B{�B{�Bz�B{�B� B�B�%B�%B�1B�7B�7B�=B�DB�VB�bB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�XB�qB��BĜBȴB��B��B��B��B��B�B�HB�sB�B��B��B	B	%B	+B	1B	DB	JB	PB	\B	VB	hB	uB	�B	�B	�B	�B	�B	�B	$�B	-B	2-B	@�B	G�B	M�B	M�B	O�B	W
B	^5B	aHB	cTB	e`B	e`B	iyB	v�B	{�B	�\B	�oB	�hB	�PB	�PB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�3B	�-B	�'B	�'B	�B	�3B	�wB	��B	�}B	�}B	�jB	�dB	��B	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�/B	�5B	�;B	�BB	�NB	�HB	�HB	�NB	�NB	�TB	�TB	�ZB	�`B	�`B	�`B	�`B	�ZB	�ZB	�ZB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
1B

=B
DB
JB
JB
JB
PB
VB
VB
VB
VB
\B
\B
\B
bB
bB
bB
bB
hB
hB
oB
oB
oB
uB
{B
�B
 �B
#�B
+B
0!B
9XB
?}B
D�B
L�B
R�B
W
B
\)B
aHB
e`B
iyB
o�B
q�B
t�B
x�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�NB
�OB
�PB
�NB
�QB
�OB
�OB
�PB
�OB
�LB
�JB
�JB
�LB
�PB
�JB
�OB
�NB
�NB
�PB
�PB
�LB
�KB
�KB
�UB
�fB
�B
��B�B5B�8B\BBhBc/B�"B�MB��B�ZB��B��BϼBпB�iB��B�xB�BBBnBB{B�B1B/�B%�B��B�%B��B��B� B.�B"�B�B�B~�BX�B;<B B�tB�]B�B�=B��B�Bz�B��B� B��B:4BK�BS�BhEB��Bx�BY�B=HB)B#B �B��B�rB�3B�SB��B��B��B{�BBeB�B4B�B
�B
�.B
��B
˞B
�B
�FB
��B
�OB
v�B
gFB
N�B
%�B
)B	��B	��B	�B	��B	�:B	��B	�B	q�B	T�B	1B	eB	"B��B�B�_B�>B�B��BϿBɚB�aB�PB�RB�^B�fB�mB�BǌBζB��B��BδBɘB�uBȓBεBζB�rBɘB��B��B�B�
B��B��BϼBͯB�^B�SB�YB�MB�)B��B��B��B��B�iB�NB�;B�B��B|�Bz�By�By�By�Bz�Bz�Bz�By�B{�B|�B{�B{�Bz�Bz�By�By�By�By�Bz�B{�B{�Bz�B{�B�B��B��B��B�B�B�B�B�!B�/B�=B�EB�IB�OB�\B�`B�nB�tB��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�1B�IB�_B�tBȌBʙB̢BϷB��B��B��B�B�IB�B��B��B	�B	�B	�B	B	B	B	#B	0B	,B	=B	IB	TB	VB	aB	tB	�B	�B	$�B	,�B	2 B	@VB	GB	M�B	M�B	O�B	V�B	^B	aB	c$B	e0B	e0B	iIB	v�B	{�B	�-B	�>B	�8B	� B	�B	�9B	�FB	�]B	�hB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�EB	�RB	�JB	�JB	�8B	�2B	�XB	�jB	�pB	�vB	�zB	�{B	�B	ȀB	ȁB	ɈB	˖B	͞B	вB	вB	гB	ѷB	ѸB	ҿB	ѹB	ѸB	ѺB	ѹB	ѸB	ѸB	ѸB	ҿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�'B	�,B	�-B	�*B	�,B	�&B	�$B	�'B	�*B	�6B	�@B	�GB	�IB	�SB	�NB	�QB	�PB	�QB	�RB	�SB	�TB	�\B	�cB	�hB	�cB	�bB	�cB	�jB	�sB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
B
B
B
B
B
!B
 B
 B
!B
%B
$B
%B
)B
,B
-B
-B
2B
2B
8B
8B
8G�O�B
CB
|B
 �B
#�B
*�B
/�B
9"B
?GB
DeB
L�B
R�B
V�B
[�B
aB
e)B
i@B
ogB
qtB
t�B
x�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.46 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451152016080714511520160807145115  AO  ARCAADJP                                                                    20150308191729    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150308191729  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150308191729  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145115  IP                  G�O�G�O�G�O�                