CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-05-28T19:15:53Z AOML 3.0 creation; 2016-08-07T21:51:29Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160528191553  20160825183417  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               |A   AO  5287_9017_124                   2C  D   APEX                            6529                            072314                          846 @ׯ�I���1   @ׯ����@0O\(��d�&�x��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    |A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�  B�  B�33B�ffB���B�  B���B�  C   C  C  C�C�C	�fC�fC�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D��fD�6fD�|�D�ɚD��3D�6fD�p D�� D�3D�C3D�ffD�ɚD�3D�C3Dڙ�D�� D�� D�<�D�fD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@θRA\)A'\)AG\)Ag\)A��A��A��A��AîAӮA�A�B�
B	�
B�
B�
B!�
B)�
B2=pB9�
BA�
BI�
BQ�
BY�
Ba�
Bi�
Bq�
By�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BиRB��B��B��B��B��B��B�Q�B�RB��B��RB��C u�Cu�Cu�C�]C�]C
\)C\)C\)Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�C u�C"u�C$u�C&u�C(u�C*u�C,u�C.u�C0u�C2u�C4u�C6u�C8u�C:u�C<u�C>u�C@u�CBu�CDu�CFu�CHu�CJu�CLu�CNu�CPu�CRu�CTu�CVu�CXu�CZu�C\u�C^u�C`u�Cbu�Cd�]Cfu�Chu�Cju�Clu�Cnu�Cpu�Cru�Ctu�Cvu�Cxu�Czu�C|u�C~u�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt�qDt�>Dy�>D��D�ED���D��RD��D�ED�~�D�θD�!�D�Q�D�uD��RD�!�D�Q�DڨRD�޸D���D�K�D�D�ۅ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ȴAվwAՕ�A�bNA�-A��A�VA�  A��A��TA��/A���A���A���A���A�ȴA�ȴA�ƨA�ĜA���AԴ9Aԟ�Aԉ7A�|�A�x�A�r�A�p�A�dZA�G�A�Aӣ�A�{A��#A�ȴA�33A���A�AмjA�=qA�z�A�33A�  AήA�z�A�33A��AͬA�r�A�/A̗�A�;dA�p�A�%A�ĜA�ZA�bNA��#A�z�AǇ+A�C�A�-A�(�A�VA��
A�bNA��A�r�A�1'A�5?A�;dA���A��-A�n�A��A��wA�ƨA��uA�t�A��!A���A��A�jA�/A��PA�33A�?}A�`BA�v�A�-A�t�A�-A��;A�|�A���A�;dA�O�A�n�A�|�A��yA�G�A�A�S�A;dAy�7AuC�An��Ak�-Aj��Ah�Ag�Ae�Ab�yA\��AX��AV�HAV �AUVAS�AOdZAKp�AIK�AH�RAG"�AD��AAl�A?XA>�DA=�A<(�A:jA8��A8=qA7�#A6�DA4��A3��A3G�A1��A1
=A0�HA0r�A/��A.�A-��A,�+A*�A*9XA)��A(A'%A& �A%�A$ZA"9XA!7LA r�A�+A�jA^5A �A�/A�A�A?}A�An�A��A?}AZAv�A�A33A
=AI�A�-A��AdZA�`A �A`BA�\AM�A�Az�A�A�PA
�uA
{A	�
A	�-A	x�A��A��Az�A=qAƨA��A��AVA�uAn�A?}A�A7LA �!@���@��\@�-@��@���@��u@�(�@�  @�t�@��y@���@��7@�%@�b@�t�@��F@��;@�1'@�b@�dZ@�
=@�o@��@�z�@�dZ@�=q@��@�u@�Z@�Z@�I�@� �@�l�@���@���@�V@���@�7@�o@㕁@�Q�@�$�@��D@ް!@�z�@�Q�@ە�@�J@أ�@��@��@׍P@��@�^5@ղ-@�/@���@�z�@���@�+@�$�@с@��@�9X@�b@�ƨ@�l�@�C�@�+@��H@Ͳ-@̼j@̬@�r�@�(�@˝�@�l�@�v�@���@ɲ-@�/@�Ĝ@�I�@�;d@Ƨ�@�J@ũ�@Ł@�7L@��@��@�z�@�l�@¸R@�~�@�E�@��@��#@�G�@���@���@�r�@�l�@�@��R@���@���@�$�@���@�@��-@��-@��-@�O�@��u@���@���@�n�@�v�@��\@���@��H@�n�@�M�@�=q@�t�@�b@���@�  @��@���@�{@���@��@��w@�C�@�^5@��@�@�x�@��m@�"�@��@��@�^5@��#@�%@��@���@���@��j@�Z@�\)@���@�`B@�b@��@�dZ@�;d@���@�n�@�{@�7L@��@�z�@� �@���@���@��\@��@��@���@��^@���@�p�@�V@���@�9X@�1@��F@�l�@��@��@���@�ff@�M�@�M�@�M�@�5?@�J@��@�@�`B@���@��/@��j@��9@��9@��@��@���@�z�@�Z@�(�@��@�ƨ@�+@�n�@��-@�?}@��@�z�@�A�@�1'@���@��@�S�@�o@��y@��R@��+@�E�@�{@���@�O�@�&�@��@��`@��j@���@��u@�r�@�I�@�(�@�b@��m@�ƨ@��P@�\)@�+@�o@��H@���@�J@�G�@�/@�&�@��@�%@�Ĝ@��D@�A�@��@��w@�"�@�ff@��T@��@�X@�&�@��`@��`@��`@���@��@�z�@�9X@��@�K�@�K�@�33@��+@�V@�V@�M�@�=q@�{@��#@��^@��@�X@�/@���@��9@��@��+@y��@pr�@i�@`  @Y�@P1'@F{@>�@9�@2�@.@(�`@"�!@5?@�!@l�@"�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�ȴAվwAՕ�A�bNA�-A��A�VA�  A��A��TA��/A���A���A���A���A�ȴA�ȴA�ƨA�ĜA���AԴ9Aԟ�Aԉ7A�|�A�x�A�r�A�p�A�dZA�G�A�Aӣ�A�{A��#A�ȴA�33A���A�AмjA�=qA�z�A�33A�  AήA�z�A�33A��AͬA�r�A�/A̗�A�;dA�p�A�%A�ĜA�ZA�bNA��#A�z�AǇ+A�C�A�-A�(�A�VA��
A�bNA��A�r�A�1'A�5?A�;dA���A��-A�n�A��A��wA�ƨA��uA�t�A��!A���A��A�jA�/A��PA�33A�?}A�`BA�v�A�-A�t�A�-A��;A�|�A���A�;dA�O�A�n�A�|�A��yA�G�A�A�S�A;dAy�7AuC�An��Ak�-Aj��Ah�Ag�Ae�Ab�yA\��AX��AV�HAV �AUVAS�AOdZAKp�AIK�AH�RAG"�AD��AAl�A?XA>�DA=�A<(�A:jA8��A8=qA7�#A6�DA4��A3��A3G�A1��A1
=A0�HA0r�A/��A.�A-��A,�+A*�A*9XA)��A(A'%A& �A%�A$ZA"9XA!7LA r�A�+A�jA^5A �A�/A�A�A?}A�An�A��A?}AZAv�A�A33A
=AI�A�-A��AdZA�`A �A`BA�\AM�A�Az�A�A�PA
�uA
{A	�
A	�-A	x�A��A��Az�A=qAƨA��A��AVA�uAn�A?}A�A7LA �!@���@��\@�-@��@���@��u@�(�@�  @�t�@��y@���@��7@�%@�b@�t�@��F@��;@�1'@�b@�dZ@�
=@�o@��@�z�@�dZ@�=q@��@�u@�Z@�Z@�I�@� �@�l�@���@���@�V@���@�7@�o@㕁@�Q�@�$�@��D@ް!@�z�@�Q�@ە�@�J@أ�@��@��@׍P@��@�^5@ղ-@�/@���@�z�@���@�+@�$�@с@��@�9X@�b@�ƨ@�l�@�C�@�+@��H@Ͳ-@̼j@̬@�r�@�(�@˝�@�l�@�v�@���@ɲ-@�/@�Ĝ@�I�@�;d@Ƨ�@�J@ũ�@Ł@�7L@��@��@�z�@�l�@¸R@�~�@�E�@��@��#@�G�@���@���@�r�@�l�@�@��R@���@���@�$�@���@�@��-@��-@��-@�O�@��u@���@���@�n�@�v�@��\@���@��H@�n�@�M�@�=q@�t�@�b@���@�  @��@���@�{@���@��@��w@�C�@�^5@��@�@�x�@��m@�"�@��@��@�^5@��#@�%@��@���@���@��j@�Z@�\)@���@�`B@�b@��@�dZ@�;d@���@�n�@�{@�7L@��@�z�@� �@���@���@��\@��@��@���@��^@���@�p�@�V@���@�9X@�1@��F@�l�@��@��@���@�ff@�M�@�M�@�M�@�5?@�J@��@�@�`B@���@��/@��j@��9@��9@��@��@���@�z�@�Z@�(�@��@�ƨ@�+@�n�@��-@�?}@��@�z�@�A�@�1'@���@��@�S�@�o@��y@��R@��+@�E�@�{@���@�O�@�&�@��@��`@��j@���@��u@�r�@�I�@�(�@�b@��m@�ƨ@��P@�\)@�+@�o@��H@���@�J@�G�@�/@�&�@��@�%@�Ĝ@��D@�A�@��@��w@�"�@�ff@��T@��@�X@�&�@��`@��`@��`@���@��@�z�@�9X@��@�K�@�K�@�33@��+@�V@�V@�M�@�=q@�{@��#@��^@��@�X@�/@���G�O�@��@��+@y��@pr�@i�@`  @Y�@P1'@F{@>�@9�@2�@.@(�`@"�!@5?@�!@l�@"�@�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ŢB
ĜB
ĜB
ĜB
ĜB
ÖB
ÖB
B
��B
�}B
�}B
�wB
�wB
�wB
�wB
�wB
�}B
�}B
�}B
�wB
�wB
�qB
�dB
�qB
�}B
��B
��B
ĜB
ĜB
��B
�}B
ƨB
��B
��B
��B
��B
�RB
�3B
��B
�hB
�DB
�=B
�%B
�B
�B
�B
�B
�\B
�bB
�B
�}B
�B�B;dBR�Bq�B~�B�7B��BƨB�sB�B�B.BD�BM�B[#B^5B^5BO�B:^B/B$�B�BB�BĜB�'B��B�RB�yB�B�B�B�?B��B�=BcTB-B#�B�B\BuB
=B
��B
�yB
�)B
��B
�B
�VB
z�B
_;B
D�B
�B	��B	��B	�dB	�XB	�'B	��B	��B	�bB	m�B	R�B	?}B	7LB	,B	�B	1B	B��B��B�B�mB�HB�)B�)B�#B�)B�
B��B��B��B��B��BȴBǮBȴBǮBƨBĜBÖBB��B�jB�^B�dB�dB�?B�-B�9B�}B�jB�9B�?B�dBB��B��B��B�
B�B�;B��B	VB	bB	{B	�B	(�B	0!B	/B	1'B	33B	6FB	:^B	49B	.B	-B	/B	0!B	1'B	0!B	/B	 �B	uB	uB	oB	�B	�B	�B	�B	�B	�B	 �B	 �B	$�B	%�B	!�B	$�B	+B	.B	49B	9XB	=qB	=qB	>wB	<jB	?}B	D�B	O�B	Q�B	T�B	W
B	XB	W
B	XB	^5B	bNB	l�B	q�B	{�B	� B	�B	�DB	�\B	�hB	�oB	�uB	�uB	�uB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�JB	�%B	�B	� B	�+B	�DB	�1B	�B	|�B	v�B	v�B	v�B	v�B	{�B	}�B	~�B	~�B	� B	�B	�B	�%B	�+B	�1B	�7B	�7B	�DB	�DB	�JB	�VB	�VB	�bB	�hB	�hB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�'B	�-B	�?B	�LB	�RB	�XB	�XB	�wB	��B	��B	��B	��B	B	ÖB	ÖB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�BB	�HB	�`B	�`B	�fB	�mB	�fB	�`B	�ZB	�TB	�HB	�HB	�NB	�mB	�mB	�fB	�mB	�sB	�fB	�`B	�`B	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B
DB
DB
DB
JB
DB
DB
JB
JB
JB
JB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
hB
oB
uB
uB
uB
uB
{B
{B
uB
uB
uB
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
,B
2-B
8RB
:^B
@�B
D�B
I�B
P�B
VB
ZB
_;B
dZB
gmB
m�B
o�B
q�B
t�B
w�B
z�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�~B
�~B
�zB
�|B
�}B
�vB
�qB
�mB
�eB
�]B
�]B
�TB
�WB
�UB
�WB
�WB
�\B
�[B
�YB
�UB
�WB
�SB
�BB
�RB
�]B
�bB
�iB
�~B
�~B
�cB
�\B
ƈB
ιB
��B
��B
ϾB
�1B
�B
��B
�HB
�$B
�B
�B
��B
��B
��B
��B
�?B
�@B
��B
�YB
�bB�B;=BR�Bq�B~�B�B�eBƀB�JB�B�B-�BDvBM�BZ�B^B^BO�B:5B.�B$�BB�B��B�oB��B��B�)B�OB�sB�B��B�B�gB�Bc(B,�B#�BUB0BIB
B
��B
�LB
��B
̡B
��B
�+B
z�B
_B
DtB
fB	��B	ʛB	�>B	�0B	�B	��B	��B	�=B	mmB	R�B	?[B	7)B	+�B	kB	B	�B��B��B�vB�JB�'B�B�B�B�	B��B��B��BϿBͳBʢBȔBǊBȖBǌBƆB�}B�tB�nB�bB�IB�>B�DB�CB� B�B�B�[B�IB�B�B�BB�lBˤB��B��B��B��B�B��B	/B	;B	UB	xB	(�B	/�B	.�B	0�B	3B	6B	:5B	4B	-�B	,�B	.�B	/�B	0�B	/�B	.�B	 �B	LB	JB	IB	XB	^B	iB	�B	�B	�B	 �B	 �B	$�B	%�B	!�B	$�B	*�B	-�B	4B	9-B	=GB	=FB	>KB	<>B	?SB	DpB	O�B	Q�B	T�B	V�B	W�B	V�B	W�B	^B	b#B	l]B	qB	{�B	�B	��B	�B	�,B	�9B	�?B	�EB	�DB	�FB	�>B	�EB	�YB	�kB	�qB	�oB	�vB	�pB	�cB	�SB	�B	��B	��B	�B	��B	�B	�B	��B	|�B	v�B	v�B	v�B	v�B	{�B	}�B	~�B	~�B	�B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�&B	�'B	�5B	�7B	�8B	�5B	�8B	�DB	�]B	�]B	�cB	�cB	�gB	�bB	�nB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�'B	�EB	�OB	�OB	�NB	�VB	�_B	�aB	�bB	�pB	ȀB	ʎB	˓B	̚B	ΧB	ΧB	͠B	͡B	͟B	��B	�B	�B	�-B	�,B	�2B	�:B	�2B	�+B	�'B	�B	�B	�B	�B	�:B	�8B	�0B	�;B	�@B	�3B	�.B	�+B	�3B	�9B	�RB	�jB	�gB	�]B	�YB	�VB	�dB	�\B	�]B	�WB	�XB	�XB	�aB	�^B	�_B	�_B	�cB	�jB	�nB	�uB	�zB	�{B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
	 B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
 B
!B
 B
"B
!B
 B
!B
&B
%B
'B
'B
#B
&B
.B
*B
-B
,B
-B
0B
9B
>B
?B
@B
AB
DB
DB
>B
BB
=B
CB
>B
JB
HB
JB
IB
XB
]B
YB
WB
^B
]B
\B
_B
bB
cB
dB
bG�O�B
{B
+�B
1�B
8B
:%B
@JB
DdB
I�B
P�B
U�B
Y�B
_B
d"B
g7B
mWB
ofB
qqB
t�B
w�B
z�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.46 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451302016080714513020160807145130  AO  ARCAADJP                                                                    20160528191553    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160528191553  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160528191553  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145130  IP                  G�O�G�O�G�O�                