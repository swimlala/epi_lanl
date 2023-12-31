CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:27Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I`   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R<   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  YP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       [   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       k   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  r   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140827  20181024140827  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               zA   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��$��}�1   @��%ffy@4rn��O��c�A�7K�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      zA   A   A   @333@�  @�  A   A   AA��Aa��A���A�  A�33A�  A���A���A���A�  B   B  B  B  B   B(  B0ffB8��B?��BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6�C8  C:  C<  C>  C@  CB  CD  CF  Cv  Cw�fCy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  D fD � D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D��D� D  D� DfD�fDfD� D  D� D  D� D  D� D��D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0�fD1fD1� D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  DE  DEy�DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D[��D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� DhfDh�fDi  Di� Dj  Dj� Dj��Dky�Dk��Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Du� Dv  Dvy�Dv��Dw� DwٚDy�D�@�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @@��@��R@ƸRA\)A#\)AD��Ad��A�z�A��A��GA��A�z�A�z�A�z�A�B �
B�
B�
B�
B �
B(�
B1=pB9��B@p�BH�
BP�
BX�
B`�
Bh�
Bp�
By=pB�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B�k�B���B�k�B�k�B�k�B�k�B�k�B�k�B�k�C 5�C5�C5�C5�C5�C
5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C5�C 5�C"5�C$5�C&5�C(O]C*5�C,5�C.5�C05�C25�C45�C6O]C85�C:5�C<5�C>5�C@5�CB5�CD5�CF5�Cv5�Cx)Cz)C|5�C~5�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C�'�C�'�C��C��C��C��C�C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�'�C��C��C��C��C��C��C��C��C��C��C��C�'�C�'�C�'�C�'�C��C��C�'�C��C��C��C��C��C�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��D �D �qDqD�qDD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD	qD	�qD
qD
�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qDqD�qD�D�qDD�qDqD�qD�D��D�D�qDqD�qDqD�qDqD�qDD�qDqD�qD qD �qD!�D!�qD"qD"�qD#qD#�qD$qD$�qD%qD%�qD&qD&��D'qD'�qD(qD(�qD)qD)�qD*qD*��D+qD+�qD,qD,�qD-qD-�qD.qD.�qD/qD/�qD0�D0��D1�D1�qD2D2�qD3qD3�qD4qD4�qD5qD5�qD6qD6�qD7qD7�qD8qD8�qD9qDEqDE�DFqDF�qDGqDG�qDHqDH�qDIqDI�qDJqDJ�qDKqDK�qDLqDL�qDM�DM�qDNqDN�qDOqDO�qDPqDP�qDQqDQ��DRqDR�qDSqDS��DTqDT�qDUqDU�qDVqDV�qDWqDW�qDXqDX�qDYqDY�qDZqDZ�qD[qD[�qD\D\�qD]qD]�qD^qD^�qD_qD_�qD`qD`�qDaqDa�qDbqDb�qDcqDc�qDdqDd�qDeqDe�qDfqDf�qDgqDg�qDh�Dh��DiqDi�qDjqDj�qDkDk�DlDl�qDmqDm�qDnqDn�qDoqDo�qDpqDp�qDqqDq�qDrqDr�qDsqDs�qDtqDt�qDuDu�qDvqDv�DwDw�qDw�Dy��D�G�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ffA�ffA�dZA�ffA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�jA�n�A�l�A�7LA�%A�/A�jA��A�x�A��yA��A͏\A��A�ZAȲ-A�bNAǟ�A�?}A�oA��mA�A�A��#AÉ7A�ZA�t�A��A�|�A��+A���A���A�Q�A�5?A�I�A�1A���A�%A�jA�K�A�jA�VA��A��!A�K�A��A��yA���A�C�A�VA�oA��HA�z�A�^5A���A�
=A���A���A��A���A���A��A��+A�9XA�hsA�v�A��A�C�A� �A�A�A��mA��;A��A�`BA���A��\A���A��DA�v�A�|�A�+A��hA���A��A��PA�9XA�=qA?}AM�AIp�AH��AHM�AFM�AD�AB��AA��A?\)A=�;A=x�A=\)A=A;�A;;dA:��A:9XA8�9A7+A3��A1��A1"�A0(�A/7LA.  A-�mA-��A-|�A-%A,r�A*�HA)C�A'?}A&r�A%��A%O�A$ĜA$�DA$A#�7A#XA"I�A 1AM�A��A��A^5AdZA��A��AhsA�AS�A�A�AȴA��A��AM�AK�AoA
A�A	��A	O�AĜAffA�mAdZAC�A`BAG�A(�AS�A&�A��AbA��AZA �9@���@��h@�1'@�(�@��w@���@��@��@��@��y@�@�z�@�l�@�?}@�@�!@�V@�=q@�$�@��@� �@䛦@�u@��@�?}@��@�9@��@��@�(�@�@�ff@�hs@��@���@�t�@�@ְ!@��#@���@�I�@�ƨ@ӥ�@ӍP@�\)@��@�@��@ҧ�@���@щ7@�x�@�x�@��@�I�@��m@�o@���@���@θR@�-@�O�@���@�1@��@��@�n�@�O�@�1@�dZ@�"�@��@��H@�E�@��@�`B@�1@�;d@��#@�/@���@��@���@�;d@���@���@��@��@�33@�ff@��-@�G�@�I�@��;@�t�@��@���@��
@���@���@���@��@�C�@��!@�^5@�^5@�=q@�{@��@�X@���@��`@���@��u@�  @�S�@�
=@���@���@��@���@�V@�Z@���@��@��@���@�^5@��R@���@�n�@���@�hs@��/@�r�@�Q�@�1@��@�C�@�33@��H@�ȴ@���@���@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@���@���@�^5@�5?@���@��@�O�@��^@���@�bN@�"�@���@�E�@��T@���@��h@�O�@��
@�n�@�9X@��F@�C�@��@��H@�ff@��@��@��@��@��#@���@���@��^@��-@���@�`B@�Q�@�t�@�n�@�-@���@�`B@�O�@�/@�r�@��m@�
=@��@�~�@�n�@���@���@���@���@��T@�x�@�V@�&�@�?}@�`B@��7@�hs@�&�@���@��@�A�@�  @��@�ƨ@��@��P@�@��y@��R@�ff@�@�@��^@���@���@���@��h@��@���@��u@�z�@�j@�9X@���@��@��w@�dZ@��H@�ȴ@��R@���@���@�~�@�J@���@�`B@�G�@�%@��D@�A�@�b@��@��@��@�;d@���@��+@�n�@�V@�$�@�@��#@���@���@��-@�@���@��f@u�h@g�V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�ffA�ffA�dZA�ffA�ffA�ffA�ffA�ffA�ffA�hsA�hsA�hsA�hsA�jA�n�A�l�A�7LA�%A�/A�jA��A�x�A��yA��A͏\A��A�ZAȲ-A�bNAǟ�A�?}A�oA��mA�A�A��#AÉ7A�ZA�t�A��A�|�A��+A���A���A�Q�A�5?A�I�A�1A���A�%A�jA�K�A�jA�VA��A��!A�K�A��A��yA���A�C�A�VA�oA��HA�z�A�^5A���A�
=A���A���A��A���A���A��A��+A�9XA�hsA�v�A��A�C�A� �A�A�A��mA��;A��A�`BA���A��\A���A��DA�v�A�|�A�+A��hA���A��A��PA�9XA�=qA?}AM�AIp�AH��AHM�AFM�AD�AB��AA��A?\)A=�;A=x�A=\)A=A;�A;;dA:��A:9XA8�9A7+A3��A1��A1"�A0(�A/7LA.  A-�mA-��A-|�A-%A,r�A*�HA)C�A'?}A&r�A%��A%O�A$ĜA$�DA$A#�7A#XA"I�A 1AM�A��A��A^5AdZA��A��AhsA�AS�A�A�AȴA��A��AM�AK�AoA
A�A	��A	O�AĜAffA�mAdZAC�A`BAG�A(�AS�A&�A��AbA��AZA �9@���@��h@�1'@�(�@��w@���@��@��@��@��y@�@�z�@�l�@�?}@�@�!@�V@�=q@�$�@��@� �@䛦@�u@��@�?}@��@�9@��@��@�(�@�@�ff@�hs@��@���@�t�@�@ְ!@��#@���@�I�@�ƨ@ӥ�@ӍP@�\)@��@�@��@ҧ�@���@щ7@�x�@�x�@��@�I�@��m@�o@���@���@θR@�-@�O�@���@�1@��@��@�n�@�O�@�1@�dZ@�"�@��@��H@�E�@��@�`B@�1@�;d@��#@�/@���@��@���@�;d@���@���@��@��@�33@�ff@��-@�G�@�I�@��;@�t�@��@���@��
@���@���@���@��@�C�@��!@�^5@�^5@�=q@�{@��@�X@���@��`@���@��u@�  @�S�@�
=@���@���@��@���@�V@�Z@���@��@��@���@�^5@��R@���@�n�@���@�hs@��/@�r�@�Q�@�1@��@�C�@�33@��H@�ȴ@���@���@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@�ȴ@���@���@�^5@�5?@���@��@�O�@��^@���@�bN@�"�@���@�E�@��T@���@��h@�O�@��
@�n�@�9X@��F@�C�@��@��H@�ff@��@��@��@��@��#@���@���@��^@��-@���@�`B@�Q�@�t�@�n�@�-@���@�`B@�O�@�/@�r�@��m@�
=@��@�~�@�n�@���@���@���@���@��T@�x�@�V@�&�@�?}@�`B@��7@�hs@�&�@���@��@�A�@�  @��@�ƨ@��@��P@�@��y@��R@�ff@�@�@��^@���@���@���@��h@��@���@��u@�z�@�j@�9X@���@��@��w@�dZ@��H@�ȴ@��R@���@���@�~�@�J@���@�`B@�G�@�%@��D@�A�@�b@��@��@��@�;d@���@��+@�n�@�V@�$�@�@��#@���@���@��-@�@���@��f@u�h@g�V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�JB�JB�JB�JB�JB�PB�PB�PB�PB�PB�PB�PB�PB�PB�PB�JB�DB�7B�7B�DB�=B�JB��B�}B��B�#B�yB��B��B��B1B�B�B�B�B �B+B-B0!B1'B2-B7LB:^BA�BG�BJ�BO�BYB^5B`BBaHBgmBk�BhsBVBI�BH�B@�B;dB8RB'�B$�B!�B�B�BhB
=B%B  B�B�B�`B�/B�B�B��BB��B��Bw�BiyBcTBaHB[#BT�BC�B"�BDB
�B
�NB
��B
�B
��B
�uB
�1B
�B
m�B
ZBv�B	VB��B��B��B�B�NB�B��B��BȴBƨBŢBĜBB��B�wB�jB�RB�9B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�PB�DB�1B�+B�B�B�B�B� B|�B{�B|�B�B�=B�{B��B�{B�uB�oB�hB�bB�bB�hB��B��B��B�B�'B�B�B�!B�'B��B��B��B��B�FB�RB�dB�wB��B��B�}B�wB�}B�}B�qB�wB�qB�XB�FB�?B�?B�FB�LB�'B�9B�jBBÖB��B��B�}B�wB��B�}B��B��BÖBǮBɺB��B��B��B��B��B�B�B�B�5B�BB�NB�NB�TB�ZB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B	B	B	B	B	%B	DB	bB	bB	oB	{B	�B	�B	�B	{B	�B	�B	�B	�B	�B	!�B	!�B	!�B	"�B	"�B	$�B	#�B	!�B	#�B	&�B	+B	0!B	2-B	8RB	C�B	G�B	H�B	K�B	L�B	N�B	Q�B	S�B	T�B	W
B	ZB	YB	YB	]/B	`BB	`BB	aHB	bNB	e`B	iyB	k�B	m�B	m�B	o�B	o�B	p�B	p�B	o�B	o�B	p�B	p�B	u�B	|�B	}�B	� B	�B	�%B	�+B	�7B	�7B	�=B	�JB	�PB	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�^B	�XB	�RB	�LB	�LB	�LB	�FB	�XB	�^B	�dB	�jB	�wB	�wB	�wB	�}B	�}B	�}B	�qB	�jB	�jB	�}B	��B	��B	��B	ÖB	ǮB	ɺB	��B	��B	��B	��B	�B	�B	�)B	�#B	�B	�#B	�5B	�;B	�;B	�HB	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B
DB
PB
B
 \B
.c111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�JB�JB�JB�JB�JB�PB�PB�PB�PB�PB�PB�PB�PB�PB�PB�JB�DB�7B�7B�DB�=B�JB��B�}B��B�#B�yB��B��B��B1B�B�B�B�B �B+B-B0!B1'B2-B7LB:^BA�BG�BJ�BO�BYB^5B`BBaHBgmBk�BhsBVBI�BH�B@�B;dB8RB'�B$�B!�B�B�BhB
=B%B  B�B�B�`B�/B�B�B��BB��B��Bw�BiyBcTBaHB[#BT�BC�B"�BDB
�B
�NB
��B
�B
��B
�uB
�1B
�B
m�B
ZBv�B	VB��B��B��B�B�NB�B��B��BȴBƨBŢBĜBB��B�wB�jB�RB�9B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�PB�DB�1B�+B�B�B�B�B� B|�B{�B|�B�B�=B�{B��B�{B�uB�oB�hB�bB�bB�hB��B��B��B�B�'B�B�B�!B�'B��B��B��B��B�FB�RB�dB�wB��B��B�}B�wB�}B�}B�qB�wB�qB�XB�FB�?B�?B�FB�LB�'B�9B�jBBÖB��B��B�}B�wB��B�}B��B��BÖBǮBɺB��B��B��B��B��B�B�B�B�5B�BB�NB�NB�TB�ZB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B	B	B	B	B	%B	DB	bB	bB	oB	{B	�B	�B	�B	{B	�B	�B	�B	�B	�B	!�B	!�B	!�B	"�B	"�B	$�B	#�B	!�B	#�B	&�B	+B	0!B	2-B	8RB	C�B	G�B	H�B	K�B	L�B	N�B	Q�B	S�B	T�B	W
B	ZB	YB	YB	]/B	`BB	`BB	aHB	bNB	e`B	iyB	k�B	m�B	m�B	o�B	o�B	p�B	p�B	o�B	o�B	p�B	p�B	u�B	|�B	}�B	� B	�B	�%B	�+B	�7B	�7B	�=B	�JB	�PB	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�^B	�XB	�RB	�LB	�LB	�LB	�FB	�XB	�^B	�dB	�jB	�wB	�wB	�wB	�}B	�}B	�}B	�qB	�jB	�jB	�}B	��B	��B	��B	ÖB	ǮB	ɺB	��B	��B	��B	��B	�B	�B	�)B	�#B	�B	�#B	�5B	�;B	�;B	�HB	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B
DB
PB
B
 \B
.c111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.21 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140827                              AO  ARCAADJP                                                                    20181024140827    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140827  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140827  QCF$                G�O�G�O�G�O�0               