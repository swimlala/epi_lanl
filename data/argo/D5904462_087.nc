CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-15T10:17:09Z AOML 3.0 creation; 2016-08-07T21:51:23Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20151115101709  20160807145123  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               WA   AO  5287_9017_087                   2C  D   APEX                            6529                            072314                          846 @�~�%*�1   @�~���!@0��C���d�I�^1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    WA   B   B   @�33@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B�  B�33B�ffB���B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C�C�fC  C  C
  C  C  C  C  C  C  C  C  C  C�C �C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�fD�3D�0 D�|�D��3D��D�FfD�p D���D�  D�L�D�i�D�� D�	�D�P Dډ�D��3D�� D�0 D�3D�y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@θRA\)A'\)AG\)AeA��A��A��A��AîAӮA�z�A�B�
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
B��B��B��B��B��RB��RB��B��B��B��B��B��B�Q�B��RB��RB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �]C�]C\)Cu�Cu�C
u�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�C�]C �]C"u�C$u�C&\)C(u�C*u�C,u�C.u�C0u�C2u�C4u�C6u�C8u�C:u�C<u�C>u�C@u�CBu�CDu�CFu�CHu�CJu�CLu�CNu�CPu�CRu�CTu�CVu�CXu�CZu�C\u�C^u�C`u�Cbu�Cdu�Cfu�Chu�Cju�Clu�Cnu�Cpu�Cru�Ctu�Cvu�Cxu�Czu�C|u�C~u�C�:�C�:�C�:�C�G�C�:�C�.C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�.C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]��D^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt�qDuDy��D��D�>�D���D���D��D�UD�~�D��RD�.�D�[�D�xRD��D�RD�^�DژRD���D���D�>�D��D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aߣ�A߮A߰!A߲-A߲-A߲-Aߴ9A߰!A߲-Aߴ9Aߴ9A߶FA߸RA߶FA߸RA߶FA߶FA߸RA߸RAߺ^A߼jA߼jA߾wA߾wA߾wA���A���A�ĜA�ƨA�ȴA�ȴA���A�ƨA�ĜA�ĜA߮AދDA�&�A�p�A�M�A���A�C�A؅A׋DA�`BAӰ!A��Aѥ�A�ȴAΧ�A�"�A͋DA�A�bAǏ\AƧ�AƩ�A��TA�"�A�ffA��A���A�?}A��A�C�A��yA��`A�C�A�jA��A�1'A���A��hA��7A��/A�\)A�?}A��HA�|�A��!A���A��A�ZA���A�bNA���A��\A��A���A�$�A�oA�ƨA�p�A�K�A��wA��#A�=qA��A�x�A��A~��Az��Avn�Ap��AnZAi�hAe�Ab�A_��A]ƨAZ�`AX�AV~�AT�ATI�AR�\AQ�APv�ANjAM�AK�AI
=AGl�AE��AD�ADVAC`BAAl�A?&�A=�-A=%A<r�A;G�A:�A9x�A8ȴA7�TA6��A6{A5��A5?}A4A2(�A0�A/�hA.��A-��A,=qA*bNA(�DA'hsA'��A#��A��AZA1A�-A�/A�A|�A~�AO�A�#A�A�A"�A33A&�A�A��A��A9XA��A\)A9XAȴA�FA �A~�A�PA(�A��AA��A��A�A
�A	K�AVA�A$�AoA$�A�7A��A�A�HAZAK�A9XA&�A�9A��A�mA��A �+@���@�p�@�?}@��@��!@��H@�t�@��@�l�@��y@��@��@�$�@���@�O�@�@��#@��#@��@�+@�-@�7@�hs@���@���@�V@�bN@�+@�D@��@��#@���@�b@��@�ȴ@���@�@���@�h@�hs@�@�dZ@�+@�@��@���@��@�M�@���@��
@�o@�^5@�V@�V@��#@�Q�@ۍP@���@ڟ�@�5?@�hs@�"�@�-@�J@��#@��@պ^@Ցh@��T@��y@�;d@�+@֧�@�%@ӍP@�l�@�K�@�v�@��T@ѡ�@��@�r�@�  @Ϯ@�;d@�+@ΰ!@�@���@ͩ�@̴9@�j@� �@�ƨ@�S�@ʟ�@�ff@�J@Ɂ@��`@���@�V@��@��@�&�@�1@�;d@�@��y@Ƨ�@��@���@ư!@�-@őh@�G�@���@���@��`@��@�Ĝ@�1'@Å@°!@�E�@���@�`B@���@�I�@��P@���@�p�@��@��@��@�K�@�E�@��@�@�7L@��@��j@���@�r�@�(�@���@�S�@�K�@�33@���@���@�n�@��+@�$�@�E�@�5?@���@���@��7@�X@���@�Ĝ@�j@�b@��P@�33@�~�@�$�@��@�X@���@��u@�r�@��@��w@��@�+@���@�E�@��T@���@�G�@��9@�j@�1'@�1@���@��F@��@���@���@��P@�S�@�v�@��#@��7@���@��j@�j@� �@��
@��F@��P@�t�@�"�@���@��R@�v�@�@��-@��@�/@��j@�j@�(�@��w@�|�@�\)@�C�@�
=@���@��+@�^5@���@���@�O�@�%@�Ĝ@��u@�(�@���@�dZ@�;d@��H@���@�V@�-@��T@�x�@�p�@�O�@�V@�I�@�b@���@���@�l�@�33@��y@��R@��\@�M�@�J@��#@��@��@��T@��7@�/@��`@��u@�j@�I�@��F@��@���@���@�@��^@�X@��@�V@��@���@�A�@�|�@�"�@��+@�$�@��@�@��7@���@���@�V@�Ĝ@}`B@r^5@j=q@c��@\(�@Q��@K@AG�@8Ĝ@1hs@+�m@%V@�y@�7@�+@n�@l�@I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aߣ�A߮A߰!A߲-A߲-A߲-Aߴ9A߰!A߲-Aߴ9Aߴ9A߶FA߸RA߶FA߸RA߶FA߶FA߸RA߸RAߺ^A߼jA߼jA߾wA߾wA߾wA���A���A�ĜA�ƨA�ȴA�ȴA���A�ƨA�ĜA�ĜA߮AދDA�&�A�p�A�M�A���A�C�A؅A׋DA�`BAӰ!A��Aѥ�A�ȴAΧ�A�"�A͋DA�A�bAǏ\AƧ�AƩ�A��TA�"�A�ffA��A���A�?}A��A�C�A��yA��`A�C�A�jA��A�1'A���A��hA��7A��/A�\)A�?}A��HA�|�A��!A���A��A�ZA���A�bNA���A��\A��A���A�$�A�oA�ƨA�p�A�K�A��wA��#A�=qA��A�x�A��A~��Az��Avn�Ap��AnZAi�hAe�Ab�A_��A]ƨAZ�`AX�AV~�AT�ATI�AR�\AQ�APv�ANjAM�AK�AI
=AGl�AE��AD�ADVAC`BAAl�A?&�A=�-A=%A<r�A;G�A:�A9x�A8ȴA7�TA6��A6{A5��A5?}A4A2(�A0�A/�hA.��A-��A,=qA*bNA(�DA'hsA'��A#��A��AZA1A�-A�/A�A|�A~�AO�A�#A�A�A"�A33A&�A�A��A��A9XA��A\)A9XAȴA�FA �A~�A�PA(�A��AA��A��A�A
�A	K�AVA�A$�AoA$�A�7A��A�A�HAZAK�A9XA&�A�9A��A�mA��A �+@���@�p�@�?}@��@��!@��H@�t�@��@�l�@��y@��@��@�$�@���@�O�@�@��#@��#@��@�+@�-@�7@�hs@���@���@�V@�bN@�+@�D@��@��#@���@�b@��@�ȴ@���@�@���@�h@�hs@�@�dZ@�+@�@��@���@��@�M�@���@��
@�o@�^5@�V@�V@��#@�Q�@ۍP@���@ڟ�@�5?@�hs@�"�@�-@�J@��#@��@պ^@Ցh@��T@��y@�;d@�+@֧�@�%@ӍP@�l�@�K�@�v�@��T@ѡ�@��@�r�@�  @Ϯ@�;d@�+@ΰ!@�@���@ͩ�@̴9@�j@� �@�ƨ@�S�@ʟ�@�ff@�J@Ɂ@��`@���@�V@��@��@�&�@�1@�;d@�@��y@Ƨ�@��@���@ư!@�-@őh@�G�@���@���@��`@��@�Ĝ@�1'@Å@°!@�E�@���@�`B@���@�I�@��P@���@�p�@��@��@��@�K�@�E�@��@�@�7L@��@��j@���@�r�@�(�@���@�S�@�K�@�33@���@���@�n�@��+@�$�@�E�@�5?@���@���@��7@�X@���@�Ĝ@�j@�b@��P@�33@�~�@�$�@��@�X@���@��u@�r�@��@��w@��@�+@���@�E�@��T@���@�G�@��9@�j@�1'@�1@���@��F@��@���@���@��P@�S�@�v�@��#@��7@���@��j@�j@� �@��
@��F@��P@�t�@�"�@���@��R@�v�@�@��-@��@�/@��j@�j@�(�@��w@�|�@�\)@�C�@�
=@���@��+@�^5@���@���@�O�@�%@�Ĝ@��u@�(�@���@�dZ@�;d@��H@���@�V@�-@��T@�x�@�p�@�O�@�V@�I�@�b@���@���@�l�@�33@��y@��R@��\@�M�@�J@��#@��@��@��T@��7@�/@��`@��u@�j@�I�@��F@��@���@���@�@��^@�X@��@�V@��@���@�A�@�|�@�"�@��+@�$�@��@�@��7G�O�@���@�V@�Ĝ@}`B@r^5@j=q@c��@\(�@Q��@K@AG�@8Ĝ@1hs@+�m@%V@�y@�7@�+@n�@l�@I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
e`B
e`B
e`B
e`B
e`B
e`B
e`B
�+B��B�NB�B�}B�RB��B��B��B�!B�mB�Bo�B��B�?B��B�}B�B��B�=B�+B�oB�=B�+B�B��B�oB}�Bz�B�BXB=qB7LB%�B
=B�fB�TB�)B��B��B�sB�HB�/B�BÖB�B�B+B1BB
��B
��B
��B
�'B
��B
��B
�B
r�B
gmB
S�B
;dB
�B
\B
JB	��B	��B	�!B	��B	x�B	hsB	P�B	?}B	/B	 �B	�B	hB		7B	B��B��B��B�B�B�B�B�yB�B�B�B�B�B�B��B	B	1B	DB	PB	uB	�B	�B	"�B	'�B	.B	2-B	2-B	5?B	;dB	F�B	H�B	N�B	N�B	P�B	D�B	:^B	)�B	!�B	)�B	�B��B��B�B�B�B��B��B	uB	/B	B�B	M�B	\)B	x�B	|�B	~�B	�B	�JB	�JB	�JB	�PB	�=B	�B	{�B	r�B	hsB	^5B	XB	R�B	I�B	D�B	<jB	<jB	9XB	.B	�B	�B	DB	B��B��B	  B	B	B	VB	uB	%�B	9XB	T�B	]/B	]/B	bNB	[#B	M�B	H�B	I�B	R�B	XB	W
B	aHB	l�B	u�B	�B	�DB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�\B	�uB	��B	��B	��B	��B	��B	�uB	�VB	�PB	�VB	�VB	�DB	�=B	�PB	�DB	�JB	��B	��B	�'B	�-B	�-B	�B	�!B	�RB	�RB	�dB	�wB	�}B	�^B	�RB	�FB	�?B	�RB	�XB	�LB	�?B	�FB	�?B	�?B	�?B	�3B	��B	��B	�B	�B	�!B	�3B	�FB	�XB	ÖB	ƨB	ƨB	ÖB	�qB	�XB	�XB	�XB	�RB	�LB	�FB	�?B	�?B	�?B	�XB	�^B	�^B	�RB	�RB	�LB	�LB	�?B	�?B	�?B	�9B	�9B	�3B	�3B	�3B	�9B	�?B	�LB	�RB	�XB	�^B	�jB	�jB	�jB	�qB	�wB	��B	ŢB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�
B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�/B	�5B	�;B	�BB	�BB	�BB	�BB	�HB	�NB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
	7B

=B
DB
JB
PB
PB
PB
\B
bB
hB
bB
bB
\B
bB
bB
bB
bB
hB
oB
oB
oB
uB
oB
hB
bB
bB
\B
bB
�B
�B
�B
�B
 �B
'�B
0!B
5?B
:^B
B�B
F�B
M�B
VB
^5B
cTB
hsB
m�B
r�B
t�B
x�B
z�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
eCB
eEB
eCB
eCB
eEB
eEB
eCB
eEB
eEB
eEB
eEB
eEB
eFB
eAB
eEB
eCB
eCB
eCB
eCB
eEB
eCB
eCB
eFB
eCB
eHB
eCB
eCB
eEB
eFB
eDB
eBB
eEB
eEB
eEB
eEB
�B�aB�)B��B�WB�,B��B��B�|B��B�GB�BoxB�_B�B�aB�VB��B��B�B�B�KB�B�B��B�]B�HB}�Bz�B��BW�B=GB7#B%�B
B�=B�*B��B��B�YB�JB�B�B��B�lB��B��B*�BB �B
��B
��B
�`B
��B
��B
�`B
��B
r�B
gEB
S�B
;<B
rB
2B
'B	��B	ʞB	� B	�^B	x�B	hSB	P�B	?\B	.�B	 �B	�B	GB		B	�B��B��B��B�B��B�pB�^B�[B�_B�_B�xB��B�|B�B��B	 �B	B	"B	-B	SB	�B	�B	"�B	'�B	-�B	2B	2B	5B	;=B	F�B	H�B	N�B	N�B	P�B	DvB	:8B	)�B	!�B	)�B	[B��B��B�B�B�B��B��B	NB	.�B	BgB	M�B	\ B	x�B	|�B	~�B	��B	�B	�!B	�B	�'B	�B	��B	{�B	r�B	hJB	^B	W�B	R�B	I�B	DrB	<AB	<@B	9,B	-�B	�B	^B	B	�B��B��B��B	 �B	�B	,B	KB	%�B	9+B	T�B	]B	]B	b"B	Z�B	M�B	H�B	I�B	R�B	W�B	V�B	aB	l^B	u�B	��B	�B	�HB	�YB	�]B	�lB	��B	��B	��B	��B	�eB	�,B	�GB	�ZB	�}B	��B	�~B	�kB	�EB	�%B	�"B	�&B	�(B	�B	�B	� B	�B	�B	�fB	��B	��B	��B	��B	��B	��B	� B	�#B	�4B	�HB	�MB	�-B	�!B	�B	�B	�!B	�'B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�&B	�dB	�tB	�vB	�cB	�?B	�&B	�)B	�'B	�B	�B	�B	�B	�B	�B	�&B	�,B	�/B	�#B	�!B	�B	�B	�B	�B	�B	�	B	�B	� B	�B	� B	�B	�B	�B	�B	�&B	�-B	�7B	�8B	�8B	�BB	�DB	�XB	�pB	ΦB	гB	дB	ѷB	ѸB	ѻB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�2B	�XB	�]B	�^B	�^B	�`B	�_B	�dB	�bB	�kB	�cB	�eB	�_B	�^B	�]B	�_B	�^B	�YB	�VB	�_B	�`B	�^B	�]B	�\B	�]B	�]B	�^B	�cB	�jB	�cB	�eB	�cB	�dB	�dB	�iB	�kB	�kB	�jB	�eB	�jB	�hB	�iB	�sB	�jB	�dB	�]B	�cB	�fB	�cB	�dB	�hB	�nB	�oB	�lB	�oB	�qB	�qB	�oB	�yB	�}B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	 B

B
B
B
B
B
B
'B
,B
1B
,B
-B
(B
,B
-B
,B
-B
0B
9B
7B
7B
=B
8B
1B
-B
,B
%B
+G�O�B
JB
IB
bB
 �B
'�B
/�B
5	B
:(B
BXB
FpB
M�B
U�B
]�B
cB
h:B
mZB
rxB
t�B
x�B
z�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.46 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451232016080714512320160807145123  AO  ARCAADJP                                                                    20151115101709    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151115101709  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151115101709  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145123  IP                  G�O�G�O�G�O�                