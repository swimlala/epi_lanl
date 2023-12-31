CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-07-04T18:02:08Z AOML 3.0 creation; 2016-08-07T21:51:31Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160704180208  20160825183417  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287_9017_131                   2C  D   APEX                            6529                            072314                          846 @׸�n]U�1   @׸Ի���@1-�d��S���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  CL�C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyS3D�� D�I�D�vfD��fD� D�C3D��3D��fD�	�D�FfD���D�ɚD��D�9�D�p D�� D�fD�L�D�3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @W
>@��@θRA\)A'\)AG\)Ag\)A��A��A��A��AîAӮA�A�B�
B	�
B�
B�
B!�
B)�
B1�
B9�
BB=pBJ=pBQ�
BY�
Ba�
Bi�
Bq�
By�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C �]Cu�Cu�Cu�Cu�C
u�Cu�Cu�Cu�CC\)Cu�Cu�Cu�Cu�Cu�C u�C"u�C$u�C&u�C(u�C*u�C,u�C.u�C0u�C2u�C4u�C6u�C8u�C:u�C<u�C>u�C@u�CBu�CDu�CFu�CHu�CJu�CLu�CNu�CPu�CRu�CTu�CVu�CXu�CZu�C\u�C^u�C`u�Cbu�Cdu�Cfu�Chu�Cju�Clu�Cnu�Cpu�Cru�Ctu�Cvu�Cxu�Czu�C|u�C~u�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�C�:�D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt��Dyp�D���D�XRD��D��D��D�Q�D���D��D�RD�UD��RD��RD��D�HRD�~�D�޸D�%D�[�D��D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A٥�A٥�A٧�AٮA٬AٮAٮA٬AٮAٰ!Aٲ-Aٰ!Aٰ!Aٟ�A�~�A�n�A�jA�hsA�^5A�I�A�K�A�G�A�C�A�A�A�?}A�=qA�=qA�;dA�5?A�5?A�/A� �A��A�jA�33A��yA׸RA�l�Aֺ^A՗�A�/A�r�A��A��A��A���A�z�A�M�A���AЗ�AϮA�/A��mA�x�A�+A��A���A̺^A�"�A��`A�dZAɓuA��A���Aũ�A�S�AøRA�bNA§�A���A���A��A��A��hA�%A��A��A�I�A��DA�&�A��A�p�A���A�S�A�;dA���A��+A��-A��A��!A��
A��A�E�A�?}A���A��jA��^A��FA��A�jA�5?A��A��yA���A���A�-A��mA��!A��A���A{�Ay�#Av$�Ao
=Am&�AkAi�TAi33Ag�PAf��Ae��Ab��A`E�A]x�A\r�A[O�AX�AV��AUAS�FAQ�AO�AKoAHffAG��AFjAC�;AA��A@$�A>��A=7LA<��A;|�A:~�A8��A7G�A5A3�hA25?A1�;A1��A1%A0{A.bA-�A+&�A)�A)�A(��A(�A&�\A%�A%��A$�9A"��A"jA!�;A!dZA ��A ffA�
AXAffA%A�;A�hA;dA
=A�A�A��AE�A�A��A�hA��A�A33A�TA�\A{A�7A��A��A�9A��A�\A9XA�-Al�AVA�A��A�uA33AQ�A��A
��Av�A��A�yAn�A��A�jA1'A��AƨA��AhsAC�AoA �`A ĜA V@�o@�-@��@��#@�X@��@�Z@�
=@�l�@�|�@���@��D@� �@��w@���@�C�@�@���@�V@�z�@�(�@�  @���@��w@��w@��F@�~�@���@�hs@�V@�@� �@�@�t�@�S�@�33@��H@��#@�j@��;@@�t�@��-@���@�+@�{@�-@�G�@�@��@��@��
@�
=@��@�=q@���@��@��T@�h@��u@�C�@�l�@�  @�n�@��@��;@�~�@�n�@�J@�J@���@��T@�@�x�@�x�@��@ج@��@ם�@�l�@��@ָR@֟�@�^5@�/@ԃ@�  @�\)@җ�@�M�@��#@�&�@���@���@У�@�Ĝ@Л�@�|�@�K�@���@�$�@���@��T@́@��@��@ʧ�@ʟ�@ʗ�@�M�@��@���@���@��`@�I�@�l�@�
=@���@��@��@�v�@��@őh@��/@�Q�@î@��@�M�@�=q@�5?@���@��@�Q�@��m@�v�@�p�@�X@�O�@�/@�%@��/@��j@��D@�bN@�1'@�|�@�ȴ@���@�ff@�5?@�$�@�=q@��@��T@��^@�x�@��j@�1@�ƨ@�ƨ@��F@��@�dZ@�K�@�"�@�@��H@��+@��@�A�@���@�\)@�o@��y@��H@��@���@�~�@��@�x�@�hs@�hs@�X@�`B@�X@�G�@�7L@��`@�j@��@�  @���@��m@���@��F@��F@�b@�b@��@���@��w@�C�@�E�@��-@�p�@��@��D@�Q�@�b@��;@��@�@�ȴ@��\@�@���@���@�z�@�  @���@�+@��@�ff@�V@�5?@�@�`B@���@��9@�Z@���@���@��@��\@�5?@�{@�@��@�@�p�@���@�A�@��m@���@�33@�@��y@���@��@���@�hs@�?}@�/@�&�@�V@���@�Q�@�Q�@�A�@�(�@��m@���@�S�@���@�ff@�j@�-@�1'@���@��`@v�+@nE�@fV@^ȴ@XA�@Nȴ@E�@>{@5�h@-�@&�+@!X@�@Q�@�m@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A٥�A٥�A٧�AٮA٬AٮAٮA٬AٮAٰ!Aٲ-Aٰ!Aٰ!Aٟ�A�~�A�n�A�jA�hsA�^5A�I�A�K�A�G�A�C�A�A�A�?}A�=qA�=qA�;dA�5?A�5?A�/A� �A��A�jA�33A��yA׸RA�l�Aֺ^A՗�A�/A�r�A��A��A��A���A�z�A�M�A���AЗ�AϮA�/A��mA�x�A�+A��A���A̺^A�"�A��`A�dZAɓuA��A���Aũ�A�S�AøRA�bNA§�A���A���A��A��A��hA�%A��A��A�I�A��DA�&�A��A�p�A���A�S�A�;dA���A��+A��-A��A��!A��
A��A�E�A�?}A���A��jA��^A��FA��A�jA�5?A��A��yA���A���A�-A��mA��!A��A���A{�Ay�#Av$�Ao
=Am&�AkAi�TAi33Ag�PAf��Ae��Ab��A`E�A]x�A\r�A[O�AX�AV��AUAS�FAQ�AO�AKoAHffAG��AFjAC�;AA��A@$�A>��A=7LA<��A;|�A:~�A8��A7G�A5A3�hA25?A1�;A1��A1%A0{A.bA-�A+&�A)�A)�A(��A(�A&�\A%�A%��A$�9A"��A"jA!�;A!dZA ��A ffA�
AXAffA%A�;A�hA;dA
=A�A�A��AE�A�A��A�hA��A�A33A�TA�\A{A�7A��A��A�9A��A�\A9XA�-Al�AVA�A��A�uA33AQ�A��A
��Av�A��A�yAn�A��A�jA1'A��AƨA��AhsAC�AoA �`A ĜA V@�o@�-@��@��#@�X@��@�Z@�
=@�l�@�|�@���@��D@� �@��w@���@�C�@�@���@�V@�z�@�(�@�  @���@��w@��w@��F@�~�@���@�hs@�V@�@� �@�@�t�@�S�@�33@��H@��#@�j@��;@@�t�@��-@���@�+@�{@�-@�G�@�@��@��@��
@�
=@��@�=q@���@��@��T@�h@��u@�C�@�l�@�  @�n�@��@��;@�~�@�n�@�J@�J@���@��T@�@�x�@�x�@��@ج@��@ם�@�l�@��@ָR@֟�@�^5@�/@ԃ@�  @�\)@җ�@�M�@��#@�&�@���@���@У�@�Ĝ@Л�@�|�@�K�@���@�$�@���@��T@́@��@��@ʧ�@ʟ�@ʗ�@�M�@��@���@���@��`@�I�@�l�@�
=@���@��@��@�v�@��@őh@��/@�Q�@î@��@�M�@�=q@�5?@���@��@�Q�@��m@�v�@�p�@�X@�O�@�/@�%@��/@��j@��D@�bN@�1'@�|�@�ȴ@���@�ff@�5?@�$�@�=q@��@��T@��^@�x�@��j@�1@�ƨ@�ƨ@��F@��@�dZ@�K�@�"�@�@��H@��+@��@�A�@���@�\)@�o@��y@��H@��@���@�~�@��@�x�@�hs@�hs@�X@�`B@�X@�G�@�7L@��`@�j@��@�  @���@��m@���@��F@��F@�b@�b@��@���@��w@�C�@�E�@��-@�p�@��@��D@�Q�@�b@��;@��@�@�ȴ@��\@�@���@���@�z�@�  @���@�+@��@�ff@�V@�5?@�@�`B@���@��9@�Z@���@���@��@��\@�5?@�{@�@��@�@�p�@���@�A�@��m@���@�33@�@��y@���@��@���@�hs@�?}@�/@�&�@�V@���@�Q�@�Q�@�A�@�(�@��m@���@�S�@���G�O�@�j@�-@�1'@���@��`@v�+@nE�@fV@^ȴ@XA�@Nȴ@E�@>{@5�h@-�@&�+@!X@�@Q�@�m@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�sB
�sB
�yB
�sB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�yB
�B
�B
�B
�B
�B
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
�B
�mB
�/B
�B
��B
��B
ȴB
�qB
�B
��B
��B
��B
�uB
�bB
�bB
�\B
�PB
�VB
�DB
�B
�B
�+B
�hB
�bB
�bB
�hB
��B
��B
��B
�;B
��BDBP�Bu�B��B��B�B�dBȴB��B�HB	7B(�B8RB<jB<jB49B-B-B+B(�B$�B�B\B	7B��B�B�BɺB�qB��B��B�\Bp�BN�B7LBVB
�B
�5B
�B
ɺB
��B
��B
�B
��B
�B
ffB
?}B
!�B
uB
\B	��B	��B	��B	B	�RB	�FB	�B	�!B	�3B	��B	�DB	x�B	z�B	�B	q�B	_;B	R�B	L�B	<jB	-B	�B	bB	JB	B��B�B�yB�`B�TB�HB�;B�)B�B��B��B��B��B��B��BɺBƨBĜB��B�qB�jB�dB�XB�LB�LB�FB�9B�-B�3B�qB�wB��BBĜBƨBƨBɺB��B��B�B�
B�B�B�B�B�B�B�/B�NB�sB�B�B��B	B	%B	1B	DB	PB	VB	VB	\B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	 �B	�B	$�B	0!B	1'B	1'B	33B	5?B	7LB	8RB	9XB	9XB	:^B	:^B	;dB	<jB	<jB	=qB	E�B	H�B	H�B	H�B	J�B	N�B	T�B	`BB	hsB	jB	k�B	w�B	|�B	�B	�7B	�DB	�JB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�3B	�LB	�dB	�jB	�dB	�dB	�dB	�XB	�^B	�^B	�jB	�jB	�qB	�wB	�qB	�^B	�RB	�LB	�FB	�?B	�FB	�LB	�RB	�LB	�9B	�XB	�qB	�^B	�FB	�3B	�B	�B	�B	�3B	�3B	�?B	�?B	�FB	�LB	�RB	�XB	�XB	�XB	�XB	�jB	��B	B	B	��B	��B	��B	�}B	��B	��B	��B	��B	B	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�;B	�;B	�BB	�HB	�HB	�HB	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�NB	�NB	�TB	�TB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�sB	�sB	�sB	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
1B
1B
	7B
1B
1B
1B
	7B
	7B
	7B

=B
PB
JB
�B
�B
"�B
,B
/B
5?B
6FB
:^B
B�B
G�B
N�B
S�B
[#B
`BB
gmB
l�B
o�B
t�B
x�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�SB
�SB
�XB
�QB
�YB
�XB
�XB
�VB
�VB
�XB
�VB
�YB
�XB
�YB
�]B
�^B
�]B
�\B
�ZB
�bB
�dB
�bB
�dB
�dB
�dB
�bB
�dB
�hB
�dB
�dB
�cB
�`B
�OB
�B
��B
��B
ϻB
ȐB
�QB
��B
��B
�qB
�iB
�SB
�BB
�DB
�:B
�-B
�4B
�#B
��B
��B
�
B
�EB
�CB
�BB
�GB
�_B
��B
ͮB
�B
��B BP�Bu�B�lB��B��B�=BȐBͬB�B	B(�B8+B<GB<CB4B,�B,�B*�B(�B$�BvB/B	B��B�B��BɐB�DB��B��B�1BpwBN�B7!B+B
�mB
�B
��B
ɑB
˝B
�XB
��B
�_B
��B
f<B
?UB
!�B
NB
4B	��B	��B	ʚB	�iB	�,B	� B	��B	��B	�B	��B	�!B	x�B	z�B	��B	q�B	_B	R�B	L�B	<IB	,�B	~B	@B	'B	�B��B��B�VB�@B�1B�(B�B�	B��B��B��BηBͳB̬BˤBəBƄB�{B�`B�NB�GB�CB�6B�)B�+B�#B�B�B�B�OB�SB�fB�nB�wBƄBƄBɗBδB��B��B��B��B��B��B��B��B��B�	B�)B�NB�\B�B��B	�B	�B	
B	B	(B	0B	/B	3B	XB	ZB	yB	�B	�B	�B	�B	�B	!�B	 �B	�B	$�B	/�B	0�B	0�B	3B	5B	7!B	8'B	9-B	9-B	:5B	:5B	;;B	<?B	<@B	=GB	EwB	H�B	H�B	H�B	J�B	N�B	T�B	`B	hGB	jRB	kXB	w�B	|�B	��B	�	B	�B	�B	�HB	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�4B	�<B	�5B	�6B	�4B	�&B	�.B	�,B	�9B	�8B	�AB	�HB	�?B	�/B	�"B	�B	�B	�B	�B	�B	�!B	�B	�B	�%B	�BB	�/B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�(B	�&B	�%B	�&B	�7B	�XB	�_B	�]B	�SB	�PB	�QB	�KB	�RB	�TB	�QB	�RB	�_B	�mB	ȂB	̛B	ΪB	ΨB	ϭB	гB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�"B	�B	�B	� B	� B	�B	�B	�!B	�"B	�"B	�"B	�#B	�"B	�B	�!B	�#B	�"B	� B	�)B	�&B	�&B	�,B	�0B	�AB	�>B	�>B	�<B	�:B	�LB	�RB	�SB	�PB	�SB	�TB	�QB	�UB	�RB	�RB	�QB	�RB	�VB	�jB	�kB	�kB	�hB	�jB	�hB	�hB	�iB	�jB	�pB	�vB	�vB	�vB	�}B	�{B	�|B	�|B	�}B	�{B	�B	�B	�B	�B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
	B


G�O�B
B
KB
qB
"�B
+�B
.�B
5	B
6B
:(B
BYB
GxB
N�B
S�B
Z�B
`B
g8B
lRB
ofB
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.46 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451312016080714513120160807145131  AO  ARCAADJP                                                                    20160704180208    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160704180208  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160704180208  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145131  IP                  G�O�G�O�G�O�                