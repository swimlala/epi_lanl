CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-07T09:15:32Z AOML 3.0 creation; 2016-06-01T00:08:26Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150907091532  20160531170826  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ~A   AO  4055_7112_126                   2C  D   APEX                            5374                            041511                          846 @�m�WkYv1   @�m� �X@:y������d!�^5?}1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ~A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH��BP  BW��B`  Bh  Bp  Bx  B�33B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(fD(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�fD���D�L�D�� D�� D���D�Y�D�|�D��3D�3D�L�D��3D�� D��D�6fDډ�D��fD��fD�@ D� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@θRA\)A'\)AG\)Ag\)A��A��A��A��AîAӮA�A�B=pB	�
B�
B�
B!�
B)�
B1�
B9�
BA�
BJ��BQ�
BYp�Ba�
Bi�
Bq�
By�
B��B�Q�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C u�Cu�Cu�Cu�Cu�C
u�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�Cu�C u�C"u�C$u�C&u�C(u�C*u�C,u�C.u�C0u�C2u�C4u�C6u�C8u�C:u�C<u�C>u�C@u�CBu�CDu�CFu�CHu�CJu�CLu�CNu�CPu�CRu�CTu�CVu�CXu�CZu�C\u�C^u�C`u�Cbu�Cdu�Cfu�Chu�Cju�Clu�Cnu�Cpu�Cru�Ctu�Cvu�Cxu�Czu�C|u�C~u�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(#�D(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt��Dy��D�RD�[�D���D�޸D�RD�hRD���D���D�!�D�[�D���D�θD�+�D�EDژRD��D�D�N�D�D�˅11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�S�A�S�AۃAۋDAۍPAۏ\Aۉ7AہA�p�A�^5A�33AڶFA�5?A���A�ƨA�bNA�1'A�oA��A���A�S�A�ZA�{Aҏ\Aя\A�ffA͟�A�ffA�\)A�v�Aư!A�;dA�\)A��#A�;dA��A���A�"�A���A��^A���A���A�A��mA��jA��A�A���A���A��uA�&�A���A�|�A���A��TA�1'A���A��+A�hsA�dZA�"�A�%A��wA���A�1A���A��#A��A���A�S�A���A�+A�ĜA�jA��A��jA�dZA�-A�1'A�/A�?}A�{A��yA�bNA�l�A�7LA�A�ZA�  A��\A�{A�ZA�
=A��;A�K�AdZA~�HA~��A}�Azr�Ax��Ax(�Aw��AwS�Aw%Av�+Au�-AsdZAr{Ao�#Am%Aj�\Ag��Af�Afr�AeAcS�AaoA_ƨA_�PA_t�A_l�A_"�A^�\A]dZA[��AXJAU��AT��AR9XAN��AMG�AL1'AKx�AK"�AKAJ�uAI��AH��AH�AG�AFȴAF=qAC��AA&�A>�A=%A<�A<bNA;�TA;�A;��A;+A:VA8�A4��A3O�A2��A2VA1��A0�jA0ZA/�wA/G�A.�9A.ZA-�wA-�7A-S�A,ȴA,�A,I�A+�A*�RA)A'7LA%S�A$bNA$(�A#�7A"jA!�#A ��A 1A�A+Az�A1A��A?}A�\A�hA��A��A�DA{A��AS�A%AA�A��At�AA��Ax�A;dA�yA��A��A�mAC�A��A�yA��A�A&�A	��A	dZA	XA	XA	XA	/Az�AA��A�uA��A��A�9A�jAȴA�RA�!A^5A�AK�A�HA��AZA�A �@��F@�E�@���@��/@���@�n�@�&�@�1'@��+@��j@�t�@�\@���@���@�ȴ@�@���@��@�!@�@��T@�-@�p�@�r�@��@�1@�=q@�V@�A�@�(�@� �@�b@��;@ץ�@ם�@�
=@�~�@���@Դ9@ӶF@�|�@��y@���@υ@͡�@ˮ@�+@�@���@�v�@�$�@���@�`B@��@���@��m@���@�V@�(�@���@�l�@���@�v�@�\)@�dZ@�+@�X@���@�j@�(�@�+@��u@���@�t�@�l�@�K�@�33@�@�@���@��@���@�?}@���@��y@��/@�(�@��P@�A�@���@�+@�ȴ@��\@�ff@�5?@��@�?}@�1'@��w@���@���@�v�@�{@��#@��-@�O�@�Ĝ@���@�C�@��R@���@�ff@�5?@�X@��w@�;d@��R@�$�@�G�@���@��!@�5?@���@�/@���@�Ĝ@��u@�r�@� �@���@�E�@�$�@��h@��@�A�@�(�@���@�|�@�"�@��R@�@���@�`B@�V@���@�r�@��m@��@���@���@��P@��P@�|�@�|�@�l�@�\)@�C�@�o@���@��+@�~�@�ff@��@���@���@�G�@��@��@��@�Q�@�1'@���@��F@�S�@�+@�@��@���@�V@�-@�$�@�@�@��T@�hs@��@� �@�ƨ@��@��P@�dZ@�"�@��\@���@��h@��`@��@�(�@�  @��w@�?}@��`@���@�Ĝ@��j@�z�@�;@��@��@|�@~�y@}�h@}V@|��@|�@|�j@{�m@x�`@xA�@vv�@u�T@u@u��@u�h@u�h@u�@up�@u�@u�h@u�h@u�h@u��@u��@up�@t�@t��@t�j@t�@t�D@tj@t9X@t(�@s��@s�@r�H@rM�@r�@q%@j�@f��@a%@Z��@Q%@Kt�@D��@=�h@:�@2-@,�@&{@�@j@ �@dZ@\)@�/@
=q@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�S�A�S�AۃAۋDAۍPAۏ\Aۉ7AہA�p�A�^5A�33AڶFA�5?A���A�ƨA�bNA�1'A�oA��A���A�S�A�ZA�{Aҏ\Aя\A�ffA͟�A�ffA�\)A�v�Aư!A�;dA�\)A��#A�;dA��A���A�"�A���A��^A���A���A�A��mA��jA��A�A���A���A��uA�&�A���A�|�A���A��TA�1'A���A��+A�hsA�dZA�"�A�%A��wA���A�1A���A��#A��A���A�S�A���A�+A�ĜA�jA��A��jA�dZA�-A�1'A�/A�?}A�{A��yA�bNA�l�A�7LA�A�ZA�  A��\A�{A�ZA�
=A��;A�K�AdZA~�HA~��A}�Azr�Ax��Ax(�Aw��AwS�Aw%Av�+Au�-AsdZAr{Ao�#Am%Aj�\Ag��Af�Afr�AeAcS�AaoA_ƨA_�PA_t�A_l�A_"�A^�\A]dZA[��AXJAU��AT��AR9XAN��AMG�AL1'AKx�AK"�AKAJ�uAI��AH��AH�AG�AFȴAF=qAC��AA&�A>�A=%A<�A<bNA;�TA;�A;��A;+A:VA8�A4��A3O�A2��A2VA1��A0�jA0ZA/�wA/G�A.�9A.ZA-�wA-�7A-S�A,ȴA,�A,I�A+�A*�RA)A'7LA%S�A$bNA$(�A#�7A"jA!�#A ��A 1A�A+Az�A1A��A?}A�\A�hA��A��A�DA{A��AS�A%AA�A��At�AA��Ax�A;dA�yA��A��A�mAC�A��A�yA��A�A&�A	��A	dZA	XA	XA	XA	/Az�AA��A�uA��A��A�9A�jAȴA�RA�!A^5A�AK�A�HA��AZA�A �@��F@�E�@���@��/@���@�n�@�&�@�1'@��+@��j@�t�@�\@���@���@�ȴ@�@���@��@�!@�@��T@�-@�p�@�r�@��@�1@�=q@�V@�A�@�(�@� �@�b@��;@ץ�@ם�@�
=@�~�@���@Դ9@ӶF@�|�@��y@���@υ@͡�@ˮ@�+@�@���@�v�@�$�@���@�`B@��@���@��m@���@�V@�(�@���@�l�@���@�v�@�\)@�dZ@�+@�X@���@�j@�(�@�+@��u@���@�t�@�l�@�K�@�33@�@�@���@��@���@�?}@���@��y@��/@�(�@��P@�A�@���@�+@�ȴ@��\@�ff@�5?@��@�?}@�1'@��w@���@���@�v�@�{@��#@��-@�O�@�Ĝ@���@�C�@��R@���@�ff@�5?@�X@��w@�;d@��R@�$�@�G�@���@��!@�5?@���@�/@���@�Ĝ@��u@�r�@� �@���@�E�@�$�@��h@��@�A�@�(�@���@�|�@�"�@��R@�@���@�`B@�V@���@�r�@��m@��@���@���@��P@��P@�|�@�|�@�l�@�\)@�C�@�o@���@��+@�~�@�ff@��@���@���@�G�@��@��@��@�Q�@�1'@���@��F@�S�@�+@�@��@���@�V@�-@�$�@�@�@��T@�hs@��@� �@�ƨ@��@��P@�dZ@�"�@��\@���@��h@��`@��@�(�@�  @��w@�?}@��`@���@�Ĝ@��j@�z�@�;@��@��@|�@~�y@}�h@}V@|��@|�@|�j@{�m@x�`@xA�@vv�@u�T@u@u��@u�h@u�h@u�@up�@u�@u�h@u�h@u�h@u��@u��@up�@t�@t��@t�j@t�@t�D@tj@t9X@t(�@s��@s�@r�H@rM�@r�@q%@j�@f��@a%@Z��@Q%@Kt�@D��@=�h@:�@2-@,�@&{@�@j@ �@dZ@\)@�/@
=q@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�/B�)B�B#�B&�B-B5?B:^B@�BF�B\)B�FBB	7B\B�B�B�B�B!�B+BN�B]/Bz�Bw�Bz�B�B�VB�JB�VB�PB�7B�B~�Bu�Bl�BaHBT�BI�B;dB33B)�B$�B"�B�B%BBBB  B��B�B�fB��BǮB�!B�Bm�B`BB^5BcTBS�BQ�BK�BB�BD�B5?B�BDBB��B�B�B�sB�NB�)B��BB�3B��B�VB}�Bk�B_;BE�B�B
=B
��B
��B
�sB
ɺB
��B
��B
��B
��B
�JB
�=B
�7B
�B
k�B
_;B
XB
R�B
O�B
L�B
G�B
>wB
+B
�B
\B	��B	�TB	��B	ƨB	��B	�RB	��B	�{B	�DB	�7B	�1B	�+B	�B	}�B	t�B	e`B	L�B	=qB	33B	$�B	�B	VB	
=B	+B	%B	B	B��B��B��B�B�B�B�TB��B��B��B��B��B��B��BȴBĜB�}B�FB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�\B�DB�%B�B�B�B~�B}�B{�By�Bx�Bw�Bv�Bu�Bs�Br�Bq�Bo�Bk�BhsBgmBffBffBffBe`BdZBbNB^5BW
BT�BT�BVBVBVBXBXBW
BT�BS�BR�BP�BL�BH�BF�BF�BE�BE�BC�B?}B=qB=qB>wB?}B@�BA�BB�BB�BC�BG�BM�BN�BM�BM�BM�BM�BL�BJ�BH�BE�BD�BB�BA�B>wB;dB>wBE�BD�BB�B@�B=qB9XB6FB1'B,B'�B#�B$�B&�B%�B%�B$�B#�B"�B&�B)�B+B,B,B,B,B+B+B)�B(�B(�B(�B)�B+B+B)�B)�B)�B,B1'B49B5?B6FB8RB9XB9XB:^B;dB;dB<jB?}B@�BA�BA�BA�BH�BQ�B]/BaHBcTBk�Bn�Bn�Bn�Bo�Bu�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bv�Bw�Bv�Bt�Bq�Bq�Bp�Bm�BjBffBe`Bk�Bk�Bk�Bk�BjBk�Bo�Bu�Bv�Bz�B�B�DB�\B�bB�oB�uB��B��B��B��B��B��B��B��B�B�!B�-B�?B�XBÖBɺB��B��B��B��B��B��B��B��B�B�5B�HB�ZB�yB�B�B�B��B��B��B	B	B	%B	+B	1B		7B	JB	PB	VB	VB	\B	\B	\B	\B	bB	bB	hB	oB	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	)�B	,B	-B	.B	/B	0!B	1'B	1'B	1'B	2-B	49B	;dB	=qB	=qB	B�B	F�B	J�B	O�B	P�B	T�B	VB	VB	VB	VB	W
B	YB	\)B	^5B	bNB	cTB	bNB	aHB	`BB	aHB	aHB	aHB	bNB	aHB	cTB	bNB	aHB	aHB	`BB	_;B	_;B	_;B	aHB	bNB	bNB	ffB	iyB	jB	p�B	s�B	v�B	y�B	z�B	|�B	}�B	}�B	~�B	~�B	~�B	� B	� B	�B	�7B	�PB	�\B	�hB	�oB	�oB	�oB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	ÖB	�B	�B
B
�B
�B
"�B
-B
33B
>wB
C�B
I�B
Q�B
VB
\)B
bNB
gmB
jB
n�B
r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�BqB#�B&�B,�B5B:5B@_BF�B\B�%B�B	B9BdBeBkB�B!�B*�BN�B]Bz�Bw�Bz�B��B�5B�'B�6B�/B�B��B~�Bu�BlhBa!BT�BI�B;=B3
B)�B$�B"�BjB�B�B�B�B��B��B�qB�<B��BǆB��B��BmdB`B^	Bc(BS�BQ�BK�BBeBDtB5B}BB�B��B�B�bB�FB�"B��B��B�gB�	B��B�+B}�BkWB_BEsB^B
B
��B
�B
�GB
ɍB
��B
��B
��B
�VB
�!B
�B
�	B
��B
k\B
_B
W�B
R�B
O�B
L�B
G�B
>LB
*�B
�B
2B	��B	�-B	ͭB	ƂB	�dB	�,B	��B	�VB	�B	�B	�B	�B	��B	}�B	t�B	e<B	L�B	=NB	3B	$�B	eB	5B	
B	B	B	�B	�B��B��B��B�B��B�bB�4B��B˥BϿBθBͲB˥BʢBȓB�~B�]B�'B��B��B��B��B��B��B��B��B��B��B�|B�vB�lB�fB�aB�bB�]B�PB�>B�'B�B��B��B��B~�B}�B{�By�Bx�Bw�Bv�Bu�Bs�Br�Bq�BoBkeBhVBgPBfKBfIBfHBeBBd;Bb0B^BV�BT�BT�BU�BU�BU�BW�BW�BV�BT�BS�BR�BP�BL�BH�BF�BF�BE�BE�BCyB?_B=TB=TB>WB?\B@dBAjBBoBBpBCxBG�BM�BN�BM�BM�BM�BM�BL�BJ�BH�BE�BD|BBnBAiB>WB;DB>XBE�BD{BBnB@cB=RB97B6%B1B+�B'�B#�B$�B&�B%�B%�B$�B#�B"�B&�B)�B*�B+�B+�B+�B+�B*�B*�B)�B(�B(�B(�B)�B*�B*�B)�B)�B)�B+�B1B4B5B6$B8/B98B96B:;B;BB;AB<IB?YB@aBAgBAeBAeBH�BQ�B]Ba$Bc0Bk`BntBnrBnsBovBu�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bv�Bw�Bv�Bt�Bq�Bq�Bp~BmkBjYBf@Be<Bk_Bk_Bk_Bk^BjYBk`BoxBu�Bv�Bz�B��B�B�5B�:B�GB�LB�YB�wB��B��B��B��B��B��B��B��B�B�B�-B�kBɏB̥BάBмB��B��B��B��B��B��B�
B�B�/B�NB�bB�vB�B��B��B��B	�B	�B	�B	�B	B			B	B	!B	(B	)B	+B	-B	0B	.B	3B	5B	;B	AB	QB	`B	bB	eB	rB	�B	�B	"�B	%�B	)�B	+�B	,�B	-�B	.�B	/�B	0�B	0�B	0�B	1�B	4B	;4B	=BB	=CB	B_B	FxB	J�B	O�B	P�B	T�B	U�B	U�B	U�B	U�B	V�B	X�B	[�B	^B	bB	c"B	bB	aB	`B	aB	aB	aB	bB	aB	c!B	bB	aB	aB	`B	_
B	_	B	_
B	aB	bB	bB	f6B	iEB	jLB	ppB	s�B	v�B	y�B	z�B	|�B	}�B	}�B	~�B	~�B	~�B	�B	�B	��B	�B	�B	�*B	�2B	�;B	�:B	�:B	�CB	�AB	�BB	�GB	�YB	�kB	�qB	��B	�`B	��B	�eB
�B
HB
vB
"�B
,�B
3 B
>CB
C]B
I�B
Q�B
U�B
[�B
bB
g5B
jFB
n_B
rv11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.46 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708262016053117082620160531170826  AO  ARCAADJP                                                                    20150907091532    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150907091532  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150907091532  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170826  IP                  G�O�G�O�G�O�                