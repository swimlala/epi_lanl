CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:50Z AOML 3.0 creation; 2016-08-07T21:36:32Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221350  20160807143632  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_028                   2C  D   APEX                            6531                            072314                          846 @�5�h@1   @�5��O�@2��Q��dV�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B���B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy�3D���D�P D�|�D��fD�  D�VfD�y�D�ɚD��D�S3D���DǶfD�fD�L�D�|�D��3D�	�D�9�D�y�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@ÅA\)A#\)AC\)Ac\)A��A��A��A��A��AѮA�A�B �
B�
B�
B�
B �
B(�
B0�
B8�
B@�
BH�
BP�
BX�
B`�
Bh�
Bp�
Bx�
B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�Bܞ�B���B�k�B�8RB�8RB�k�B�k�B�k�B�k�C 5�C5�C5�CO]C5�C
5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C 5�C"5�C$5�C&5�C(5�C*5�C,5�C.5�C05�C25�C45�C65�C85�C:5�C<5�C>5�C@5�CB5�CD5�CF5�CH5�CJ5�CL5�CN5�CP5�CR5�CT5�CVO]CX5�CZ5�C\5�C^5�C`5�Cb5�Cd5�Cf5�Ch5�Cj5�Cl5�Cn5�Cp5�Cr5�Ct5�Cv5�Cx5�Cz5�C|5�C~5�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�'�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D qD �qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD qD �qD!qD!�qD"qD"�qD#qD#�qD$qD$��D%qD%�qD&qD&�qD'qD'�qD(qD(�qD)qD)�qD*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3�D3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQ�DQ�qDRqDR�qDSqDS�qDTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi�qDjqDj�qDkqDk�qDlqDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt�qDt�qDy��D�ӅD�V�D���D��D��D�]D��RD��RD��D�Y�D��RDǽD�D�S�Dڃ�D���D�RD�@RD�RD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aˏ\AˑhAˑhAˍPA˕�A˗�Aˏ\Aˏ\Aˏ\AˑhA˝�A˛�Aˡ�A˟�A˟�A˗�A˛�A˝�A˟�Aˡ�AˮA˰!A˲-A˲-A˲-A˲-AˮAˬAˮA˧�Aˡ�Aˡ�Aˣ�A˥�Aˡ�A˟�A˗�Aˇ+A�x�A�n�A�XA�?}A�$�A��A��`A���Aʰ!AʓuA�dZA�$�A�VA��Aɰ!AɍPA�A�A���A�JA��A���A���A�-A��FA���A��A�A�5?A���A�O�A�XA��PA�l�A���A��A��A���A���A��wA�|�A��jA��DA���A��A��DA��A�`BA�;dA�JA��RA���A�7LA�dZA�t�A�VA�hsA�=qA�
=A��wA�(�A�5?A��#A�/A���A|v�Az�Aw�;Ar��Am�Af��Ab�DA`M�A]��AY��AX�DAW�AVE�AS�^AP�DAM�^AMdZAKƨAJJAHĜAG��AE+ADE�AC��AB��AB5?AA�-A@�jA?�A=S�A<VA;p�A9��A8��A7�7A5��A2��A0bNA-�-A,  A)+A&��A&bNA%oA#t�A"  Al�A�!AG�Ar�A��AXA��A�A�A^5A��A��A��A�A��A�TA�HAt�A�HA��A�A��A�DA�DAA
E�A	&�AAt�A
=A�A�/AĜA��A�A5?A9XA;dAM�A`BA�HA��A-A|�A I�@��7@���@��
@�S�@��y@��!@�n�@���@�I�@��@��F@�33@��@�Ĝ@�9X@��H@�^@�X@�1@�V@�-@���@�h@��`@���@띲@�dZ@��H@�-@��m@�-@�Q�@�w@�R@◍@�=q@��@�1@߮@�
=@ޟ�@���@�I�@�l�@��@�t�@��@ԛ�@�b@Ӿw@�S�@�{@�X@�V@д9@��m@ΰ!@�E�@�-@ͺ^@�&�@���@�z�@��@�+@��@�@ʧ�@�J@Ɂ@�X@�&�@Ȭ@�I�@�  @��@�|�@�\)@�dZ@�dZ@�S�@�"�@�@Ƨ�@�5?@��T@�`B@��@��`@���@ļj@ļj@Ĵ9@ċD@Ĵ9@Ĭ@�Q�@��m@��;@��;@��
@î@�t�@�S�@�@�@�{@���@�@��@�O�@���@���@�j@�ƨ@�dZ@��y@���@�=q@�E�@�5?@�J@�@�J@��@�V@�~�@�n�@�~�@�V@��T@��h@�@��h@���@���@���@�@�O�@��`@�  @��@�S�@�o@���@���@��\@��#@���@��@���@�j@�1'@��@��@���@��@��^@���@�A�@�1@��@�|�@��R@��@���@�%@���@���@�z�@�bN@�A�@�(�@���@�l�@�=q@���@��7@��@��@�9X@�ƨ@�l�@�K�@��@���@�~�@�@���@�hs@�V@���@�r�@���@��w@���@��@�33@���@���@�~�@�=q@�{@�@��@��-@�G�@�/@��@���@�Ĝ@�Q�@���@���@��P@�t�@���@��+@�-@��T@���@�hs@�7L@�/@��`@��u@�9X@�|�@�~�@�^5@�n�@�n�@�ff@���@��`@�Ĝ@���@��D@���@��j@��@��D@��@��m@�ƨ@��@�dZ@��H@��!@�ff@�=q@���@�O�@�Ĝ@��@��`@�z�@���@�1'@���@��u@�C�@��y@��@�ȴ@���@�%@�z�@�j@�Z@�A�@�b@��@��
@���@��w@�C�@�o@�+@�"�@��y@���@�ff@�n�@�^5@��@��7@�x�@�p�@�X@�&�@��/@���@�j@�(�@���@�"�@���@���@|j@r�!@h �@]�-@T�@M�h@G
=@A�7@:��@4�/@-�@(b@#��@�-@G�@/@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aˏ\AˑhAˑhAˍPA˕�A˗�Aˏ\Aˏ\Aˏ\AˑhA˝�A˛�Aˡ�A˟�A˟�A˗�A˛�A˝�A˟�Aˡ�AˮA˰!A˲-A˲-A˲-A˲-AˮAˬAˮA˧�Aˡ�Aˡ�Aˣ�A˥�Aˡ�A˟�A˗�Aˇ+A�x�A�n�A�XA�?}A�$�A��A��`A���Aʰ!AʓuA�dZA�$�A�VA��Aɰ!AɍPA�A�A���A�JA��A���A���A�-A��FA���A��A�A�5?A���A�O�A�XA��PA�l�A���A��A��A���A���A��wA�|�A��jA��DA���A��A��DA��A�`BA�;dA�JA��RA���A�7LA�dZA�t�A�VA�hsA�=qA�
=A��wA�(�A�5?A��#A�/A���A|v�Az�Aw�;Ar��Am�Af��Ab�DA`M�A]��AY��AX�DAW�AVE�AS�^AP�DAM�^AMdZAKƨAJJAHĜAG��AE+ADE�AC��AB��AB5?AA�-A@�jA?�A=S�A<VA;p�A9��A8��A7�7A5��A2��A0bNA-�-A,  A)+A&��A&bNA%oA#t�A"  Al�A�!AG�Ar�A��AXA��A�A�A^5A��A��A��A�A��A�TA�HAt�A�HA��A�A��A�DA�DAA
E�A	&�AAt�A
=A�A�/AĜA��A�A5?A9XA;dAM�A`BA�HA��A-A|�A I�@��7@���@��
@�S�@��y@��!@�n�@���@�I�@��@��F@�33@��@�Ĝ@�9X@��H@�^@�X@�1@�V@�-@���@�h@��`@���@띲@�dZ@��H@�-@��m@�-@�Q�@�w@�R@◍@�=q@��@�1@߮@�
=@ޟ�@���@�I�@�l�@��@�t�@��@ԛ�@�b@Ӿw@�S�@�{@�X@�V@д9@��m@ΰ!@�E�@�-@ͺ^@�&�@���@�z�@��@�+@��@�@ʧ�@�J@Ɂ@�X@�&�@Ȭ@�I�@�  @��@�|�@�\)@�dZ@�dZ@�S�@�"�@�@Ƨ�@�5?@��T@�`B@��@��`@���@ļj@ļj@Ĵ9@ċD@Ĵ9@Ĭ@�Q�@��m@��;@��;@��
@î@�t�@�S�@�@�@�{@���@�@��@�O�@���@���@�j@�ƨ@�dZ@��y@���@�=q@�E�@�5?@�J@�@�J@��@�V@�~�@�n�@�~�@�V@��T@��h@�@��h@���@���@���@�@�O�@��`@�  @��@�S�@�o@���@���@��\@��#@���@��@���@�j@�1'@��@��@���@��@��^@���@�A�@�1@��@�|�@��R@��@���@�%@���@���@�z�@�bN@�A�@�(�@���@�l�@�=q@���@��7@��@��@�9X@�ƨ@�l�@�K�@��@���@�~�@�@���@�hs@�V@���@�r�@���@��w@���@��@�33@���@���@�~�@�=q@�{@�@��@��-@�G�@�/@��@���@�Ĝ@�Q�@���@���@��P@�t�@���@��+@�-@��T@���@�hs@�7L@�/@��`@��u@�9X@�|�@�~�@�^5@�n�@�n�@�ff@���@��`@�Ĝ@���@��D@���@��j@��@��D@��@��m@�ƨ@��@�dZ@��H@��!@�ff@�=q@���@�O�@�Ĝ@��@��`@�z�@���@�1'@���@��u@�C�@��y@��@�ȴ@���@�%@�z�@�j@�Z@�A�@�b@��@��
@���@��w@�C�@�o@�+@�"�@��y@���@�ff@�n�@�^5@��@��7@�x�@�p�@�X@�&�@��/@���@�j@�(�G�O�@�"�@���@���@|j@r�!@h �@]�-@T�@M�h@G
=@A�7@:��@4�/@-�@(b@#��@�-@G�@/@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�fB�`B�fB�fB�`B�`B�`B�`B�`B�`B�`B�`B�`B�`B�`B�`B�ZB�ZB�`B�`B�mB�mB�sB�sB�yB�yB�B�B�B�B�B�B�B�B�B��B��BB1BJBbBPB	7B1BBBBVBoB�B2-B<jB?}B@�B@�BI�BW
BffB{�B�uB��B��B��B��B��B��B��B��B��B�bB�JB�7B�BaHB9XBB�VBJ�B49B%�B
��B
�sB
�/B
��B
��B
�qB
�!B
�B
��BE�BcTBo�BQ�B'�B
�B
�B
��B
�bB
� B
p�B
]/B
�B	��B	�qB	��B	�7B	e`B	H�B	.B	�B	hB	B��B��B�B�NB�
B��B��BŢB��B�qB�RB�?B�3B�-B�'B�B�B�B�B�'B�!B�'B�!B�B��B��B��B�B�'B�!B�3B�?B�9B�'B�-B�3B�9B�FB�FB�LB�FB�RB�XB�^B�^B�dB�dB�RB�LB�XB�RB�^B�jB�qB�qB�dB�qB�dB�jB�jB�jB�}B�}B�qB�wB�}B��B��B��B��B��B��BÖB��B��B�B�)B�)B�#B�#B�B�B�#B�B�B�B�B�B�B�/B�/B�/B�/B�/B�5B�5B�;B�;B�;B�TB�mB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B��B��B	B	+B	1B	DB	PB	VB	bB	oB	�B	�B	�B	�B	 �B	&�B	(�B	)�B	.B	1'B	33B	49B	8RB	:^B	:^B	:^B	;dB	<jB	@�B	H�B	G�B	G�B	H�B	H�B	J�B	K�B	L�B	M�B	M�B	O�B	S�B	W
B	\)B	`BB	aHB	aHB	bNB	cTB	hsB	jB	m�B	p�B	y�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�+B	�=B	�DB	�PB	�VB	�\B	�hB	�oB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�-B	�9B	�?B	�FB	�XB	�dB	�dB	�jB	�jB	�jB	�jB	�jB	�qB	�wB	�wB	�}B	�}B	�}B	�}B	��B	��B	��B	��B	ǮB	ƨB	ŢB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�;B	�NB	�ZB	�`B	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
PB
B
PB
�B
�B
 �B
)�B
49B
:^B
@�B
E�B
I�B
O�B
T�B
YB
_;B
bNB
ffB
iyB
n�B
q�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�TB�MB�UB�SB�MB�MB�MB�MB�MB�OB�MB�OB�MB�KB�MB�MB�HB�IB�OB�MB�[B�YB�bB�`B�gB�iB�zB�B�|B�B�B�B�B�B�B��B��BBB7BRB@B	'BBB�BBCB^B�B2B<YB?lB@pB@rBI�BV�BfSB{�B�gB��B��B��B��B��B��B��B�}B�pB�QB�5B�%B�Ba2B9GB �B�FBJ�B4%B%�B
��B
�cB
�B
��B
̼B
�_B
�B
�
B
��BE�Bc@Bo�BQ�B'�B
�B
��B
��B
�OB
�B
p�B
]B
~B	��B	�hB	��B	�,B	eVB	H�B	.B	�B	cB	 B��B��B�B�KB�B��B��BŝB�B�nB�PB�<B�1B�*B�$B�B�B�B�B�!B�B�#B�B�
B��B��B��B�B�%B�B�0B�<B�2B�$B�)B�.B�5B�AB�AB�EB�AB�MB�TB�XB�YB�^B�^B�LB�DB�RB�KB�[B�dB�lB�jB�^B�jB�]B�eB�gB�dB�xB�tB�iB�nB�uB�|B�yB�zB�|B�{B��BÎB��B��B�B�#B� B�B�B�B�B�B�B�B�B�B�B�B�'B�%B�%B�'B�(B�.B�+B�1B�1B�4B�LB�dB�|B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B��B��B	B	B	'B	8B	DB	JB	XB	bB	�B	�B	�B	�B	 �B	&�B	(�B	)�B	.B	1B	3#B	4+B	8CB	:SB	:OB	:RB	;VB	<]B	@tB	H�B	G�B	G�B	H�B	H�B	J�B	K�B	L�B	M�B	M�B	O�B	S�B	V�B	\B	`2B	a8B	a8B	b?B	cEB	hcB	jnB	m�B	p�B	y�B	}�B	~�B	�B	��B	��B	��B	��B	�
B	�B	�-B	�2B	�@B	�DB	�HB	�WB	�_B	�_B	�kB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�(B	�+B	�4B	�HB	�SB	�RB	�XB	�VB	�XB	�VB	�WB	�_B	�gB	�dB	�jB	�gB	�kB	�lB	�tB	�rB	�xB	�wB	ǚB	ƕB	ŏB	ŏB	ƔB	ǜB	ʱB	ʮB	˵B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�;B	�HB	�LB	�RB	�XB	�^B	�dB	�gB	�jB	�jB	�kB	�mB	�iB	�fB	�gB	�iB	�|B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
	B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�G�O�B
B
<B
lB
�B
 �B
)�B
4!B
:IB
@kB
E�B
I�B
O�B
T�B
Y B
_$B
b8B
fNB
ibB
n~B
q�B
t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.21 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436322016080714363220160807143632  AO  ARCAADJP                                                                    20150226221350    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221350  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221350  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143632  IP                  G�O�G�O�G�O�                