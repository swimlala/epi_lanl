CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:08Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024140808  20181024140808  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׳��1   @׳�I���@3�t�j~��c����+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @���@���A   A   A@  A`  A�  A�  A���A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D ��D� D��Dy�D  D� D  D� D  D� D  D� DfD� D  D� D	  D	� D
  D
� DfD� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D�fD  D� D  D�fD   D y�D!  D!� D"  D"� D#  D#y�D$  D$�fD%  D%� D&  D&y�D&��D'� D'��D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-�fD.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D>  D>� D?  D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DFy�DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DLy�DM  DM� DNfDN� DO  DO� DO��DPy�DP��DQ� DR  DRy�DR��DS� DS��DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZ� D[fD[� D[��D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dmy�Dm��Dn� Do  Doy�Do��Dpy�Dq  Dq� Dr  Dr� DsfDs�fDt  Dt� Du  Du� Du��Dv� Dw  Dw� DwٚDy�D�%11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�  A��A!��AA��Aa��A���A���A���A���A���A���A���A���B ffBffBffBffB ffB(ffB0ffB8ffB@ffBHffBPffBX��B`ffBhffBpffBxffB�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>34C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`  Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|  C~�C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D fD �fD  D�fD  D� DfD�fDfD�fDfD�fDfD�fD�D�fDfD�fD	fD	�fD
fD
�fD�D�fD�D�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD�fDfD��D�D�fDfD�fDfD��DfD�fDfD��D fD � D!fD!�fD"fD"�fD#fD#� D$fD$��D%fD%�fD&fD&� D'  D'�fD(  D(�fD)fD)�fD*fD*�fD+fD+�fD,fD,�fD-fD-��D.fD.�fD/fD/�fD0fD0�fD1fD1��D2fD2�fD3fD3�fD4fD4�fD5fD5�fD6fD6�fD7fD7�fD8fD8�fD9fD9�fD:fD:�fD;fD;�fD<fD<� D=fD=�fD>fD>�fD?fD?� D@fD@�fDAfDA�fDBfDB�fDCfDC�fDDfDD�fDEfDE�fDFfDF� DGfDG�fDH�DH�fDIfDI�fDJfDJ�fDKfDK�fDLfDL� DMfDM�fDN�DN�fDOfDO�fDP  DP� DQ  DQ�fDRfDR� DS  DS�fDT  DT�fDUfDU�fDVfDV�fDWfDW�fDXfDX��DYfDY�fDZfDZ�fD[�D[�fD\  D\�fD]�D]�fD^fD^�fD_fD_�fD`fD`�fDafDa��DbfDb�fDcfDc�fDdfDd�fDefDe�fDffDf�fDgfDg�fDhfDh�fDifDi�fDjfDj�fDkfDk�fDlfDl�fDm  Dm� Dn  Dn�fDofDo� Dp  Dp� DqfDq�fDrfDr�fDs�Ds��DtfDt�fDufDu�fDv  Dv�fDwfDw�fDw� Dy��D�(R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A� �A�$�A�&�A�(�A�+A�(�A�-A�&�A��A�
=A�{A��;A�jA��A��/A�p�A��AѸRA�33A�+A�~�A�A�jA͏\A͇+A�O�A�K�AʃAʝ�Aʴ9AʁA�VA�~�Aʩ�Aʥ�A�C�Aɕ�A�dZA�^5A��A�O�A�z�A���A�r�A�33A���A�ZA�~�A�-A�bNAøRAÃA�l�A�bNA�/A���A�x�A��`A��\A�ȴA���A�{A�XA�l�A��A�Q�A�I�A�?}A�-A�hsA��jA��A�ffA���A�Q�A���A��A��A�K�A��A�l�A��/A�1'A�?}A�
=A��HA��DA�I�A�;dA��hA��^A��HA�
=A���A��HA�A�^5A���A�`BA��yA���A�bA��A���A�&�A�\)A��A��wA�1'A�Q�A�p�A��\A��HA�`BA}C�Ax�DAu7LAs�ApjAmG�AljAkAj1Ah�uAgAf��Af��Ab�/A]S�AYAV��AR�uAP(�AN�+ANI�AN(�ANAM/AK��AK;dAJ  AG��AE`BAD1AC�ABffAA/A@^5A?�A=�^A;��A;&�A:E�A8�DA6ȴA5��A3�wA1+A/��A.ZA-��A-`BA,�HA,�DA,A*-A)`BA&��A%�hA$M�A#ƨA#C�A"�A!A �A�
AS�A;dA��A�hA�RA-AhsA�A��A�AQ�AA�hAVA��A  A�A��A��A�An�A33A+A�\A�A�HAx�A/A	��A	&�A�DA��A�A&�A�A��A"�A=qA��AC�A��A I�@�33@���@��R@��@���@�
=@��
@�@��@�O�@�bN@���@�"�@��@��@�1@�\)@��@���@��@��@�Ĝ@�Z@��
@�@�&�@��H@�V@�Ĝ@�@٩�@�X@�?}@��@�I�@�;d@�M�@�E�@�V@�V@Ձ@Ӯ@�^5@�&�@��;@�M�@��T@Ͳ-@�G�@̬@�9X@�|�@��@ɺ^@�V@�b@�dZ@�~�@ũ�@�x�@�hs@�&�@�Ĝ@�b@�K�@���@���@�z�@�Q�@��m@�t�@�@��!@�n�@���@�%@�I�@���@���@�S�@�~�@��@��-@���@� �@���@�S�@�@�ȴ@���@�n�@�~�@�$�@���@�Ĝ@�  @��m@��
@�ƨ@��F@��@��P@�t�@�l�@�@�ff@��T@��h@���@��j@��@��y@�V@��@��P@��y@���@�ȴ@���@��y@��@�?}@��/@���@�9X@��@�\)@�\)@�S�@�C�@�
=@��H@���@���@�ff@�J@��-@�hs@��@��@��/@���@��9@��u@�r�@�I�@��;@�t�@�o@�ȴ@�-@���@�?}@���@��@���@��@�Q�@�1'@� �@���@�"�@���@�^5@�$�@�J@���@�Ĝ@���@�z�@�j@�Z@�A�@� �@��@��;@���@���@�dZ@���@�M�@�-@�J@��-@�p�@�?}@���@��u@�Z@�Z@�I�@���@�K�@��@��\@���@�/@���@���@�j@�Q�@�(�@�1@��
@��@�\)@���@�v�@���@��`@��D@�Z@�(�@���@�dZ@�;d@�33@�33@�33@�+@�"�@��H@�V@�@��-@�?}@��/@��@�Q�@��m@�ƨ@��P@�33@��y@���@�~�@�ff@�ff@�^5@�$�@���@��^@���@��7@�x�@�hs@�G�@�G�@�/@�7L@���@��`@���@�r�@�9X@� �@�b@��@��w@��F@�l�@��@�ff@�{@���@�7L@���@�j@��@�;d@�"�@��@���@�ȴ@���@�~@o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A� �A�$�A�&�A�(�A�+A�(�A�-A�&�A��A�
=A�{A��;A�jA��A��/A�p�A��AѸRA�33A�+A�~�A�A�jA͏\A͇+A�O�A�K�AʃAʝ�Aʴ9AʁA�VA�~�Aʩ�Aʥ�A�C�Aɕ�A�dZA�^5A��A�O�A�z�A���A�r�A�33A���A�ZA�~�A�-A�bNAøRAÃA�l�A�bNA�/A���A�x�A��`A��\A�ȴA���A�{A�XA�l�A��A�Q�A�I�A�?}A�-A�hsA��jA��A�ffA���A�Q�A���A��A��A�K�A��A�l�A��/A�1'A�?}A�
=A��HA��DA�I�A�;dA��hA��^A��HA�
=A���A��HA�A�^5A���A�`BA��yA���A�bA��A���A�&�A�\)A��A��wA�1'A�Q�A�p�A��\A��HA�`BA}C�Ax�DAu7LAs�ApjAmG�AljAkAj1Ah�uAgAf��Af��Ab�/A]S�AYAV��AR�uAP(�AN�+ANI�AN(�ANAM/AK��AK;dAJ  AG��AE`BAD1AC�ABffAA/A@^5A?�A=�^A;��A;&�A:E�A8�DA6ȴA5��A3�wA1+A/��A.ZA-��A-`BA,�HA,�DA,A*-A)`BA&��A%�hA$M�A#ƨA#C�A"�A!A �A�
AS�A;dA��A�hA�RA-AhsA�A��A�AQ�AA�hAVA��A  A�A��A��A�An�A33A+A�\A�A�HAx�A/A	��A	&�A�DA��A�A&�A�A��A"�A=qA��AC�A��A I�@�33@���@��R@��@���@�
=@��
@�@��@�O�@�bN@���@�"�@��@��@�1@�\)@��@���@��@��@�Ĝ@�Z@��
@�@�&�@��H@�V@�Ĝ@�@٩�@�X@�?}@��@�I�@�;d@�M�@�E�@�V@�V@Ձ@Ӯ@�^5@�&�@��;@�M�@��T@Ͳ-@�G�@̬@�9X@�|�@��@ɺ^@�V@�b@�dZ@�~�@ũ�@�x�@�hs@�&�@�Ĝ@�b@�K�@���@���@�z�@�Q�@��m@�t�@�@��!@�n�@���@�%@�I�@���@���@�S�@�~�@��@��-@���@� �@���@�S�@�@�ȴ@���@�n�@�~�@�$�@���@�Ĝ@�  @��m@��
@�ƨ@��F@��@��P@�t�@�l�@�@�ff@��T@��h@���@��j@��@��y@�V@��@��P@��y@���@�ȴ@���@��y@��@�?}@��/@���@�9X@��@�\)@�\)@�S�@�C�@�
=@��H@���@���@�ff@�J@��-@�hs@��@��@��/@���@��9@��u@�r�@�I�@��;@�t�@�o@�ȴ@�-@���@�?}@���@��@���@��@�Q�@�1'@� �@���@�"�@���@�^5@�$�@�J@���@�Ĝ@���@�z�@�j@�Z@�A�@� �@��@��;@���@���@�dZ@���@�M�@�-@�J@��-@�p�@�?}@���@��u@�Z@�Z@�I�@���@�K�@��@��\@���@�/@���@���@�j@�Q�@�(�@�1@��
@��@�\)@���@�v�@���@��`@��D@�Z@�(�@���@�dZ@�;d@�33@�33@�33@�+@�"�@��H@�V@�@��-@�?}@��/@��@�Q�@��m@�ƨ@��P@�33@��y@���@�~�@�ff@�ff@�^5@�$�@���@��^@���@��7@�x�@�hs@�G�@�G�@�/@�7L@���@��`@���@�r�@�9X@� �@�b@��@��w@��F@�l�@��@�ff@�{@���@�7L@���@�j@��@�;d@�"�@��@���@�ȴ@���@�~@o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
'�B
9XB
D�B
J�B
H�B
M�B
G�B
I�B
M�B
N�B
/B
=qB
H�B
.B
B
)�B
F�B
n�B
�B
��B
��B
�B
�jB
�B
�B\BoBDB&�B-B'�B'�B8RBE�B� B��B�?B�FB�FB�FB�FB�RB�RB�LB�B�3B��B	7B33B6FB0!B"�B�B�B�B�B1'B33B2-B1'B.B)�B�BDB  B��B�B�B�`B�#B��B�;B�B�5BȴB�'B��B�Bq�BbNB[#BF�B<jB8RB/B�BDB
��B
�5B
ÖB
�9B
�B
��B
�JB
�B
u�B
]/B
N�B
?}B
)�B
�B
+B	�ZB	��B	ǮB	�FB	��B	��B	��B	�uB	�7B	�B	|�B	v�B	^5B	<jB	)�B	�B	B��B�B�B�B�B�B�B�B�mB�5B��B��B��BȴBÖB��B�dB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�\B�bB�bB�hB�oB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�'B�'B�3B�9B�FB�LB�LB�FB�LB�XB�^B�^B�^B�XB�^B�qB��BÖBƨB��B��B��B��B��B��B��B��B�B�
B�)B�5B�BB�TB�TB�TB�ZB�`B�mB�mB�B�B�B�B�B�B�B�B��B��B��B	B	B	B	B	1B	
=B	\B	�B	�B	�B	 �B	$�B	%�B	&�B	+B	/B	49B	49B	49B	6FB	7LB	8RB	9XB	9XB	9XB	:^B	:^B	:^B	;dB	@�B	I�B	N�B	P�B	Q�B	VB	W
B	W
B	^5B	bNB	dZB	ffB	k�B	n�B	s�B	t�B	z�B	�B	�B	�7B	�VB	�\B	�bB	�bB	�bB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�?B	�FB	�RB	�^B	�qB	�qB	�}B	��B	B	ÖB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�fB	�fB	�`B	�TB	�NB	�`B	�mB	�mB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
JB
JB
PB
PB
\B
bB
hB
oB
oB
oB
oB
uB
�B
'�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
'�B
9XB
D�B
J�B
H�B
M�B
G�B
I�B
M�B
N�B
/B
=qB
H�B
.B
B
)�B
F�B
n�B
�B
��B
��B
�B
�jB
�B
�B\BoBDB&�B-B'�B'�B8RBE�B� B��B�?B�FB�FB�FB�FB�RB�RB�LB�B�3B��B	7B33B6FB0!B"�B�B�B�B�B1'B33B2-B1'B.B)�B�BDB  B��B�B�B�`B�#B��B�;B�B�5BȴB�'B��B�Bq�BbNB[#BF�B<jB8RB/B�BDB
��B
�5B
ÖB
�9B
�B
��B
�JB
�B
u�B
]/B
N�B
?}B
)�B
�B
+B	�ZB	��B	ǮB	�FB	��B	��B	��B	�uB	�7B	�B	|�B	v�B	^5B	<jB	)�B	�B	B��B�B�B�B�B�B�B�B�mB�5B��B��B��BȴBÖB��B�dB�3B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�\B�bB�bB�hB�oB�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�'B�'B�3B�9B�FB�LB�LB�FB�LB�XB�^B�^B�^B�XB�^B�qB��BÖBƨB��B��B��B��B��B��B��B��B�B�
B�)B�5B�BB�TB�TB�TB�ZB�`B�mB�mB�B�B�B�B�B�B�B�B��B��B��B	B	B	B	B	1B	
=B	\B	�B	�B	�B	 �B	$�B	%�B	&�B	+B	/B	49B	49B	49B	6FB	7LB	8RB	9XB	9XB	9XB	:^B	:^B	:^B	;dB	@�B	I�B	N�B	P�B	Q�B	VB	W
B	W
B	^5B	bNB	dZB	ffB	k�B	n�B	s�B	t�B	z�B	�B	�B	�7B	�VB	�\B	�bB	�bB	�bB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�?B	�FB	�RB	�^B	�qB	�qB	�}B	��B	B	ÖB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�fB	�fB	�`B	�TB	�NB	�`B	�mB	�mB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
JB
JB
PB
PB
\B
bB
hB
oB
oB
oB
oB
uB
�B
'�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.10 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140808                              AO  ARCAADJP                                                                    20181024140808    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140808  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140808  QCF$                G�O�G�O�G�O�0               