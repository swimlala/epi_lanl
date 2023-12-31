CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:21Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140821  20181024140821  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               _A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��d��1   @��e�i�@3�p��
=�c�x���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      _A   A   A   @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BW��B_��Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�fC  C�C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4�C6�C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl�Cn  Cp  Cr  Ct�Cv�Cx  Cz�C|  C}�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D � D  D�fD  D� D  D� D  D�fDfD� D  D� D  D� D  D� D��D	� D
  D
� D  D� D  D� D  D� DfD�fD  D� D  D� DfD�fD  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D(��D)y�D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D2��D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DKfDK�fDL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DRy�DS  DS� DT  DT� DUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[y�D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDjfDj� DkfDk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Do��Dpy�Dq  Dq� Dr  Dr� DsfDs� Dt  Dty�Du  Du� Dv  Dv� Dw  Dw�fDw�3Dy��D�MqD��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@ÅA\)A#\)AC\)Ac\)A��A��A��A��A��AѮA�A�B=pB�
B�
B�
B �
B(�
B0�
B8�
B@�
BH�
BP�
BXp�B`p�Bh�
Bp�
Bx�
B�k�B�k�B�k�B�k�B�k�B�k�B�k�B���B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B���B�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�C5�C)C5�CO]CO]C 5�C"5�C$5�C&5�C(5�C*5�C,5�C.5�C05�C2O]C4O]C6O]C85�C:5�C<5�C>5�C@5�CB5�CD5�CF5�CH5�CJ5�CL5�CN5�CP5�CR)CT5�CV5�CX5�CZ5�C\5�C^5�C`5�Cb5�Cd5�Cf5�Ch)Cj5�ClO]Cn5�Cp5�Cr5�CtO]CvO]Cx5�CzO]C|5�C~)C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C�'�C��C��C�'�C�'�C�'�C�'�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�'�C�'�C��C��C��C��C��C��C��C��C��C��C�C��C��C�'�C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��D qD �qDqD��DqD�qDqD�qDqD��D�D�qDqD�qDqD�qDqD�qD	D	�qD
qD
�qDqD�qDqD�qDqD�qD�D��DqD�qDqD�qD�D��DqD�qD�D�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD��DqD�qDqD�qD qD �qD!qD!��D"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&�qD'qD'�qD(qD(�qD)D)�D*qD*�qD+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0qD0�qD1qD1�qD2qD2�qD3D3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qD9�qD:qD:�qD;qD;�qD<qD<�qD=qD=�qD>qD>�qD?qD?�qD@qD@�qDAqDA�qDBqDB�qDCqDC�qDDqDD�qDEqDE�qDFqDF�qDGqDG��DHqDH�qDIqDI�qDJqDJ�qDK�DK��DLqDL�qDMqDM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ�qDRqDR�DSqDS�qDTqDT�qDU�DU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�D\qD\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDhqDh�qDiqDi��Dj�Dj�qDk�Dk��DlqDl�qDmqDm�qDnqDn�qDoqDo�DpDp�DqqDq�qDrqDr�qDs�Ds�qDtqDt�DuqDu�qDvqDv�qDwqDw��Dx �Dy�4D�T)D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A߼jA߾wA߾wA߼jA߼jAߺ^A߸RA߬A�l�A�%A��A��Aް!Aޗ�AދDAރA�n�A�K�Aݧ�A��/A���A؛�A�;dAӉ7Aҡ�AѴ9A�%A��A�S�A˴9Aʝ�A�VA��/AȍPA��/A�Q�A�O�Aź^Aŉ7A�O�A�bA���A�C�A�ȴA��9A��yA��^A��A� �A�bNA��A���A��A�r�A�A��PA��A�A�A�ffA���A�p�A�"�A���A�hsA��FA�S�A���A�ZA�n�A�S�A��+A���A�5?A�^5A�A�A���A�`BA���A�1A���A�ffA��A��A�  A�=qA�A��A�ZA���A�1'A���A���A��uA��
A�-A�-A���A�+A�7LA�M�A}��Az�AzbAz��Az(�Ay�Aw�Au7LAr��Ap�!Amx�Al�Ak�PAiVAg��Af��Ae�;Ac�A`M�A^�A\�A[�AY�AW%AR�\AN~�ALbAJȴAIS�AH=qAF��AE�#AD�yAC��ABĜAA&�A@z�A@�DA?��A>  A<�!A<=qA;;dA:^5A8bNA7�FA6M�A4�A2VA.�A*�yA'A%�wA$M�A#�#A#t�A"��A"  A!��A!
=A �yA VA�jAJA��A�hA�7Ap�A��A��A��A��Av�A$�A�A�mA�TA��AM�Al�Az�A=qA�A��A�FA�A7LA�An�A �A7LA	��AJAQ�AVA9XA��A�A M�@�n�@��+@�Q�@���@�@�l�@�E�@���@���@�n�@�7@��@�D@�1@�S�@��@噚@�@�C�@�ff@��@�(�@�b@�b@��@�1@��;@��
@���@�{@�A�@���@���@�ff@ٺ^@؋D@֗�@�$�@���@���@ѩ�@�V@ЋD@��;@�=q@��`@̣�@�1'@�t�@�^5@��@ɉ7@���@ǥ�@ļj@�@�V@�-@��@��@�`B@���@��F@��H@��@��@�V@��R@�K�@���@��;@���@��!@�n�@�5?@�@�7L@��`@��u@�z�@�Q�@�(�@���@��@�@��^@��@��9@���@��@�1'@�b@���@��@�+@���@�ȴ@���@��\@�^5@��@��@���@���@�~�@�v�@�n�@�^5@�M�@��-@�O�@�V@���@���@�Ĝ@��D@�I�@�l�@�~�@�n�@�M�@��@��-@��-@��h@��7@�x�@�`B@�G�@�/@��@��@���@�Z@��@�|�@�33@�
=@�@��H@��@��H@��H@�ȴ@���@���@��+@�~�@�ff@�V@�@�@��h@�7L@���@�Ĝ@�Ĝ@��D@��@��w@��P@�\)@�33@�o@��@���@�=q@��@���@�p�@�%@�z�@�I�@�A�@�9X@� �@��;@���@�dZ@���@�5?@��@��#@��^@�X@���@��u@���@���@�t�@�
=@��H@��y@��y@��y@��y@���@�n�@�^5@�-@���@���@�ƨ@���@���@��P@��@�t�@�
=@��y@��R@�ff@���@��h@��@���@���@��9@��@���@���@�C�@�o@��H@���@���@�n�@�{@��^@��^@�hs@��@�Ĝ@���@�z�@�j@�r�@��@��@��
@���@��P@�@���@�ff@���@���@�hs@�?}@��@�V@���@��`@�Ĝ@���@�z�@�r�@�r�@�r�@�r�@�j@�j@�Z@��
@��@���@�|�@�dZ@�C�@�
=@��@��T@��^@�p�@��u@�(�@��F@���@���@�X@��@��j@�1'@�  @�ƨ@�\)@��@�@��@���@��+@�ff@�ff@�5?@�-@��#@��@u7L@c1�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A߼jA߾wA߾wA߼jA߼jAߺ^A߸RA߬A�l�A�%A��A��Aް!Aޗ�AދDAރA�n�A�K�Aݧ�A��/A���A؛�A�;dAӉ7Aҡ�AѴ9A�%A��A�S�A˴9Aʝ�A�VA��/AȍPA��/A�Q�A�O�Aź^Aŉ7A�O�A�bA���A�C�A�ȴA��9A��yA��^A��A� �A�bNA��A���A��A�r�A�A��PA��A�A�A�ffA���A�p�A�"�A���A�hsA��FA�S�A���A�ZA�n�A�S�A��+A���A�5?A�^5A�A�A���A�`BA���A�1A���A�ffA��A��A�  A�=qA�A��A�ZA���A�1'A���A���A��uA��
A�-A�-A���A�+A�7LA�M�A}��Az�AzbAz��Az(�Ay�Aw�Au7LAr��Ap�!Amx�Al�Ak�PAiVAg��Af��Ae�;Ac�A`M�A^�A\�A[�AY�AW%AR�\AN~�ALbAJȴAIS�AH=qAF��AE�#AD�yAC��ABĜAA&�A@z�A@�DA?��A>  A<�!A<=qA;;dA:^5A8bNA7�FA6M�A4�A2VA.�A*�yA'A%�wA$M�A#�#A#t�A"��A"  A!��A!
=A �yA VA�jAJA��A�hA�7Ap�A��A��A��A��Av�A$�A�A�mA�TA��AM�Al�Az�A=qA�A��A�FA�A7LA�An�A �A7LA	��AJAQ�AVA9XA��A�A M�@�n�@��+@�Q�@���@�@�l�@�E�@���@���@�n�@�7@��@�D@�1@�S�@��@噚@�@�C�@�ff@��@�(�@�b@�b@��@�1@��;@��
@���@�{@�A�@���@���@�ff@ٺ^@؋D@֗�@�$�@���@���@ѩ�@�V@ЋD@��;@�=q@��`@̣�@�1'@�t�@�^5@��@ɉ7@���@ǥ�@ļj@�@�V@�-@��@��@�`B@���@��F@��H@��@��@�V@��R@�K�@���@��;@���@��!@�n�@�5?@�@�7L@��`@��u@�z�@�Q�@�(�@���@��@�@��^@��@��9@���@��@�1'@�b@���@��@�+@���@�ȴ@���@��\@�^5@��@��@���@���@�~�@�v�@�n�@�^5@�M�@��-@�O�@�V@���@���@�Ĝ@��D@�I�@�l�@�~�@�n�@�M�@��@��-@��-@��h@��7@�x�@�`B@�G�@�/@��@��@���@�Z@��@�|�@�33@�
=@�@��H@��@��H@��H@�ȴ@���@���@��+@�~�@�ff@�V@�@�@��h@�7L@���@�Ĝ@�Ĝ@��D@��@��w@��P@�\)@�33@�o@��@���@�=q@��@���@�p�@�%@�z�@�I�@�A�@�9X@� �@��;@���@�dZ@���@�5?@��@��#@��^@�X@���@��u@���@���@�t�@�
=@��H@��y@��y@��y@��y@���@�n�@�^5@�-@���@���@�ƨ@���@���@��P@��@�t�@�
=@��y@��R@�ff@���@��h@��@���@���@��9@��@���@���@�C�@�o@��H@���@���@�n�@�{@��^@��^@�hs@��@�Ĝ@���@�z�@�j@�r�@��@��@��
@���@��P@�@���@�ff@���@���@�hs@�?}@��@�V@���@��`@�Ĝ@���@�z�@�r�@�r�@�r�@�r�@�j@�j@�Z@��
@��@���@�|�@�dZ@�C�@�
=@��@��T@��^@�p�@��u@�(�@��F@���@���@�X@��@��j@�1'@�  @�ƨ@�\)@��@�@��@���@��+@�ff@�ff@�5?@�-@��#@��@u7L@c1�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bk�Bk�Bk�BjBk�Bk�Bk�BiyBffBcTBbNBaHB\)BYBXBXBW
BVBN�B<jB'�B'�B.B�7B��B��B�yB��B�B�BB��B�mBB/B;dBC�BR�BZB]/B_;B]/BaHBgmBs�B�B�\B�oB�DB~�B�DB�VB�uB��B��B��B�B�B�!B�!B�3B�RB�RB�?B�B�B�B��B��B�\B|�Bl�BaHBS�BH�BI�B:^B�BB��B�B��B��B�+BgmBC�B+B
�B
ĜB
ŢB
�-B
v�B
49B
'�B
%�B
/B
1'B
8RB
^5B
K�B
@�B
B	�ZB	�B
�B
-B
�B
1B	��B	�sB	�B	�wB	�LB	�B	��B	�B	�!B	��B	��B	�B	p�B	gmB	W
B	VB	G�B	(�B	%B��B�B�B�mB�fB�TB�HB�)B�
B��B�B�B�B��B��B��BȴBŢB��B�qB�RB�-B�B��B��B�{B�hB�oB�hB�bB�JB�7B�+B�+B�+B�%B�%B�1B�1B�1B�1B�+B�+B�%B�JB�oB�hB�bB��B��B�uB�\B�hB�hB�oB�oB�oB�oB�oB�hB�hB�hB�oB��B�\B�1B�B}�B}�B|�By�Bq�Bm�Bv�B{�B|�B{�B{�B�DB�VB�PB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�9B�3B�XB�qB�}BÖBŢBǮB��B��B��B��B��B��B�B�B�B��B��B��B�
B�#B�5B�HB�ZB�TB�NB�/B�)B�#B�#B�B�
B��B��B��B��B��B�)B�ZB�B�B��B��B	%B	VB	bB	bB	oB	�B	�B	$�B	%�B	&�B	(�B	-B	33B	5?B	6FB	=qB	A�B	E�B	D�B	E�B	I�B	K�B	N�B	P�B	Q�B	R�B	T�B	[#B	^5B	dZB	hsB	l�B	m�B	n�B	o�B	o�B	p�B	u�B	z�B	}�B	~�B	� B	�B	�%B	�+B	�PB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�'B	�-B	�3B	�9B	�9B	�FB	�XB	�dB	�dB	�dB	�dB	�jB	�qB	�qB	�wB	�wB	�}B	��B	B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�;B	�BB	�BB	�BB	�HB	�HB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
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
+B

=B

=B

=B

=B
PB
VB
VB
bB
oB
oB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
{B
�B
MB
'RB
6�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bk�Bk�Bk�BjBk�Bk�Bk�BiyBffBcTBbNBaHB\)BYBXBXBW
BVBN�B<jB'�B'�B.B�7B��B��B�yB��B�B�BB��B�mBB/B;dBC�BR�BZB]/B_;B]/BaHBgmBs�B�B�\B�oB�DB~�B�DB�VB�uB��B��B��B�B�B�!B�!B�3B�RB�RB�?B�B�B�B��B��B�\B|�Bl�BaHBS�BH�BI�B:^B�BB��B�B��B��B�+BgmBC�B+B
�B
ĜB
ŢB
�-B
v�B
49B
'�B
%�B
/B
1'B
8RB
^5B
K�B
@�B
B	�ZB	�B
�B
-B
�B
1B	��B	�sB	�B	�wB	�LB	�B	��B	�B	�!B	��B	��B	�B	p�B	gmB	W
B	VB	G�B	(�B	%B��B�B�B�mB�fB�TB�HB�)B�
B��B�B�B�B��B��B��BȴBŢB��B�qB�RB�-B�B��B��B�{B�hB�oB�hB�bB�JB�7B�+B�+B�+B�%B�%B�1B�1B�1B�1B�+B�+B�%B�JB�oB�hB�bB��B��B�uB�\B�hB�hB�oB�oB�oB�oB�oB�hB�hB�hB�oB��B�\B�1B�B}�B}�B|�By�Bq�Bm�Bv�B{�B|�B{�B{�B�DB�VB�PB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�9B�3B�XB�qB�}BÖBŢBǮB��B��B��B��B��B��B�B�B�B��B��B��B�
B�#B�5B�HB�ZB�TB�NB�/B�)B�#B�#B�B�
B��B��B��B��B��B�)B�ZB�B�B��B��B	%B	VB	bB	bB	oB	�B	�B	$�B	%�B	&�B	(�B	-B	33B	5?B	6FB	=qB	A�B	E�B	D�B	E�B	I�B	K�B	N�B	P�B	Q�B	R�B	T�B	[#B	^5B	dZB	hsB	l�B	m�B	n�B	o�B	o�B	p�B	u�B	z�B	}�B	~�B	� B	�B	�%B	�+B	�PB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�'B	�-B	�3B	�9B	�9B	�FB	�XB	�dB	�dB	�dB	�dB	�jB	�qB	�qB	�wB	�wB	�}B	��B	B	ÖB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�;B	�BB	�BB	�BB	�HB	�HB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
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
+B

=B

=B

=B

=B
PB
VB
VB
bB
oB
oB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
{B
�B
MB
'RB
6�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.21 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140821                              AO  ARCAADJP                                                                    20181024140821    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140821  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140821  QCF$                G�O�G�O�G�O�0               