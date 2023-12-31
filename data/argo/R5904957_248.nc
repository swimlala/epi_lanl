CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:51Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140851  20181024140851  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @����q1   @���DDV�@5�t�j~��d�Q�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B���B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  Cz  C|  C}�fC�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD� D��Dy�D  D�fD  D� D  D� D  D� D  D�fDfD� D	  D	� D
  D
� D  D�fD  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%y�D%��D&� D'  D'� D(  D(� D)  D)y�D*  D*� D+  D+�fD,fD,� D-  D-� D.  D.� D/  D/�fD0  D0�fD1  D1y�D1��D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?y�D@  D@� DA  DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH�fDI  DI� DJfDJ�fDK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDTfDT� DUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_fD_� D`  D`� D`��Da� Db  Db�fDc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�3Dy�D�H 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@\AG�A!G�AAG�AaG�A���A���A���A��
A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�\)B�(�B�(�B�(�B���B���C {C{C{C{C{C
{C{C{C{C{C{C{C{C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{Cz{C|{C}��C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
=C�
=C�
C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
=C�
=C�
=C�
=C��pC�
=C�
=C�
=C�
=C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
=C�
=C�
=C�
C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C��pC�
=C��pC�
=C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C��pC��pC�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
=C�
=C�
=C�
=C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=D D �D�D�D��D~�DD��DD�DD�DD�DD��D�D�D	D	�D
D
�DD��DD�DD�DD�D��D�DD�DD�DD�DD�DD�D�D�DD�DD�D��D�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$��D%D%~�D%��D&�D'D'�D(D(�D)D)~�D*D*�D+D+��D,�D,�D-D-�D.D.�D/D/��D0D0��D1D1~�D1��D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?~�D@D@�DADA�DB�DB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH��DIDI�DJ�DJ��DKDK�DLDL�DMDM��DNDN�DODO�DPDP�DQDQ�DRDR�DSDS��DT�DT�DU�DU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_�D_�D`D`�D`��Da�DbDb��DcDc�DdDd�DeDe�DfDf��DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�Dw�RDy�>D�J�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�
=A�JA�1A�JA�
=A�1A�A�%A�
=A�oA��A��A�$�A�$�A�"�A�/A�+A�"�A�"�A�&�A� �A�"�A� �A��A��A��A�+A�+A�-A�+A�(�A�(�A�(�A�(�A�(�A�&�A�"�A��A��`A�~�A�Q�A�
=A�t�A���A��AĮAčPA��A��A�A�1A�hsA���A�;dA���A�r�A�ZA�ȴA�VA�ĜA�p�A���A��A��HA� �A�oA�=qA���A�^5A��A���A�&�A��;A�G�A��A���A��+A�|�A�"�A���A�ffA��HA�~�A�VA�G�A��\A��A�
=A���A�A��FA�$�A�ƨA��uA�l�A�1'A��A�bNA�  A��FAq|�Aq%Ap��AnȴAk\)AihsAh��AgVAc+A`  A\��AZ��AZbNAZQ�AZ=qAY?}AWK�AS��ARȴAQS�AM�mAIS�AH-AGt�AD�+AC�FA@��A>�9A=�A;��A9p�A9C�A9�A7�A6�DA6(�A5O�A41'A3�hA2bNA1dZA/�A/%A.�A,�A,9XA+p�A*�`A)�A)�PA)C�A(�+A'��A&��A&M�A$M�A"ĜA!�A!�wA!��A!\)A �A;dA�A33A"�A5?Ax�A�A��A�A�wAjA/A��AA�A�A$�AjA
ZA	�A	�A	��A�;A�A5?AA��A��A��A�7AK�A�A��AO�@���@�ȴ@�v�@�M�@��D@�dZ@�@�M�@�hs@�;d@�@��7@�/@���@�F@�\@�V@�h@��@�|�@��@�S�@�j@���@�n�@�%@���@��@��@�  @�
=@ڏ\@��@�%@��m@֟�@���@�`B@���@Լj@ԛ�@Ӆ@�=q@ѡ�@д9@�I�@��m@Ϯ@�=q@���@̓u@�A�@�C�@�J@��@�b@�dZ@���@ź^@��@�Z@�1@�dZ@��H@���@��7@�hs@�O�@�&�@��@��@�Q�@��@�n�@���@���@���@���@��+@�@���@��`@�Z@� �@��;@�;d@�V@��#@��@��@��m@�l�@���@�J@�&�@��@�I�@�ƨ@�+@��@��@�"�@��@�n�@��T@�?}@�/@���@���@�z�@�b@��m@���@���@��@�dZ@�\)@�33@�+@��@���@���@�v�@�-@�@�?}@���@��@�S�@��@�^5@�X@�%@��@��j@��@�Z@�9X@�(�@��@��;@���@�J@��T@��h@�G�@�%@��D@�b@��P@�
=@�n�@���@�bN@���@���@�E�@��@��#@���@��@��#@�p�@��@��@���@�/@�X@�O�@�O�@�?}@��@���@���@��D@�Z@���@�Q�@��m@���@�K�@�+@���@���@�~�@���@��@�hs@��@�Q�@�1'@��w@���@�M�@�%@�V@�X@�X@��`@��@�z�@�r�@�Z@�1'@�b@���@��m@�t�@�33@�o@�
=@���@�ff@�5?@�{@��T@���@��^@���@�p�@�7L@�7L@�%@�r�@��@��@�\)@���@�v�@�V@�-@��@��@���@�@��-@�hs@��@��@��D@��u@��@�I�@�  @�ƨ@��@���@��w@��F@�t�@�o@�ff@�{@��@��#@��^@�`B@��@���@��9@���@�K�@�"�@���@��y@��H@�ȴ@�v�@��T@�G�@��@���@��`@���@�A�@��@;d@}��@m��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�
=A�JA�1A�JA�
=A�1A�A�%A�
=A�oA��A��A�$�A�$�A�"�A�/A�+A�"�A�"�A�&�A� �A�"�A� �A��A��A��A�+A�+A�-A�+A�(�A�(�A�(�A�(�A�(�A�&�A�"�A��A��`A�~�A�Q�A�
=A�t�A���A��AĮAčPA��A��A�A�1A�hsA���A�;dA���A�r�A�ZA�ȴA�VA�ĜA�p�A���A��A��HA� �A�oA�=qA���A�^5A��A���A�&�A��;A�G�A��A���A��+A�|�A�"�A���A�ffA��HA�~�A�VA�G�A��\A��A�
=A���A�A��FA�$�A�ƨA��uA�l�A�1'A��A�bNA�  A��FAq|�Aq%Ap��AnȴAk\)AihsAh��AgVAc+A`  A\��AZ��AZbNAZQ�AZ=qAY?}AWK�AS��ARȴAQS�AM�mAIS�AH-AGt�AD�+AC�FA@��A>�9A=�A;��A9p�A9C�A9�A7�A6�DA6(�A5O�A41'A3�hA2bNA1dZA/�A/%A.�A,�A,9XA+p�A*�`A)�A)�PA)C�A(�+A'��A&��A&M�A$M�A"ĜA!�A!�wA!��A!\)A �A;dA�A33A"�A5?Ax�A�A��A�A�wAjA/A��AA�A�A$�AjA
ZA	�A	�A	��A�;A�A5?AA��A��A��A�7AK�A�A��AO�@���@�ȴ@�v�@�M�@��D@�dZ@�@�M�@�hs@�;d@�@��7@�/@���@�F@�\@�V@�h@��@�|�@��@�S�@�j@���@�n�@�%@���@��@��@�  @�
=@ڏ\@��@�%@��m@֟�@���@�`B@���@Լj@ԛ�@Ӆ@�=q@ѡ�@д9@�I�@��m@Ϯ@�=q@���@̓u@�A�@�C�@�J@��@�b@�dZ@���@ź^@��@�Z@�1@�dZ@��H@���@��7@�hs@�O�@�&�@��@��@�Q�@��@�n�@���@���@���@���@��+@�@���@��`@�Z@� �@��;@�;d@�V@��#@��@��@��m@�l�@���@�J@�&�@��@�I�@�ƨ@�+@��@��@�"�@��@�n�@��T@�?}@�/@���@���@�z�@�b@��m@���@���@��@�dZ@�\)@�33@�+@��@���@���@�v�@�-@�@�?}@���@��@�S�@��@�^5@�X@�%@��@��j@��@�Z@�9X@�(�@��@��;@���@�J@��T@��h@�G�@�%@��D@�b@��P@�
=@�n�@���@�bN@���@���@�E�@��@��#@���@��@��#@�p�@��@��@���@�/@�X@�O�@�O�@�?}@��@���@���@��D@�Z@���@�Q�@��m@���@�K�@�+@���@���@�~�@���@��@�hs@��@�Q�@�1'@��w@���@�M�@�%@�V@�X@�X@��`@��@�z�@�r�@�Z@�1'@�b@���@��m@�t�@�33@�o@�
=@���@�ff@�5?@�{@��T@���@��^@���@�p�@�7L@�7L@�%@�r�@��@��@�\)@���@�v�@�V@�-@��@��@���@�@��-@�hs@��@��@��D@��u@��@�I�@�  @�ƨ@��@���@��w@��F@�t�@�o@�ff@�{@��@��#@��^@�`B@��@���@��9@���@�K�@�"�@���@��y@��H@�ȴ@�v�@��T@�G�@��@���@��`@���@�A�@��@;d@}��@m��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�#B
�#B
�#B
�)B
�)B
�)B
�)B
�)B
�)B
�#B
�#B
�)B
�)B
�)B
�/B
�5B
�HB
�TB
�TB
��B�BdZBǮB�sB��BbB�B�B'�B+BA�BaHBffBhsBgmBhsBy�Bv�Br�Bl�Bo�B}�B�PB�PB�hB�bB�oB�VB�oB��B��B��B��B��B�uB�VB�Bw�Br�Bm�BbNBP�B?}B49B/B+B(�B"�B$�B �BhBbBB�B�HB��B�bB�Bt�BdZBYBE�B4u�B	�B	�B	�fB	�B	B	�?B	�B	��B	�B	r�B	aHB	XB	VB	T�B	S�B	M�B	C�B	2-B	-B	"�B	oB	B��B��B�B�sB�5B�
B��B��BǮBŢBĜB��B�qB�jB�^B�LB�9B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�VB�DB�=B�7B�+B�B�B}�Bz�Bv�Bs�Br�Br�Br�Bq�Br�Bq�Bn�Bk�BjBiyBhsBffBdZBcTBbNBaHBbNBbNBaHBaHB`BB`BB`BB_;B^5BaHBcTBcTBe`BhsBk�Bk�BiyBiyBjBjBjBl�Bm�Bm�Bp�Bx�B}�B{�B�B�B}�B|�Bz�Bs�Bp�Bz�B�B~�B�B�B�B�B�B�B�B�%B�1B�DB�JB�VB�bB�bB�bB�oB�{B�uB�{B��B��B��B��B��B��B��B�B�9B�RB�jB�wB��BƨBǮBɺBɺB��B��B��B��B��B��B��B��B�B�
B�B�BB�TB�ZB�fB�sB�B�B�B�B�B�B�B�B�B��B��B	B	PB	VB	bB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	(�B	(�B	.B	33B	7LB	<jB	?}B	B�B	E�B	G�B	H�B	H�B	I�B	J�B	L�B	O�B	R�B	XB	]/B	^5B	]/B	[#B	YB	XB	W
B	VB	W
B	W
B	XB	XB	XB	ZB	ZB	ZB	ZB	ZB	_;B	dZB	dZB	e`B	ffB	jB	n�B	q�B	s�B	v�B	u�B	r�B	o�B	m�B	l�B	k�B	k�B	l�B	l�B	n�B	t�B	y�B	|�B	� B	�+B	�DB	�VB	�\B	�\B	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�B	�B	�'B	�-B	�3B	�?B	�?B	�?B	�RB	�XB	�^B	�dB	�jB	�jB	�jB	�jB	�wB	��B	��B	��B	��B	B	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�BB	�HB	�ZB	�fB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�)B
�)B
�)B
�)B
�)B
�)B
�)B
�#B
�#B
�#B
�)B
�)B
�)B
�)B
�)B
�)B
�#B
�#B
�)B
�)B
�)B
�/B
�5B
�HB
�TB
�TB
��B�BdZBǮB�sB��BbB�B�B'�B+BA�BaHBffBhsBgmBhsBy�Bv�Br�Bl�Bo�B}�B�PB�PB�hB�bB�oB�VB�oB��B��B��B��B��B�uB�VB�Bw�Br�Bm�BbNBP�B?}B49B/B+B(�B"�B$�B �BhBbBB�B�HB��B�bB�Bt�BdZBYBE�B4u�B	�B	�B	�fB	�B	B	�?B	�B	��B	�B	r�B	aHB	XB	VB	T�B	S�B	M�B	C�B	2-B	-B	"�B	oB	B��B��B�B�sB�5B�
B��B��BǮBŢBĜB��B�qB�jB�^B�LB�9B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�VB�DB�=B�7B�+B�B�B}�Bz�Bv�Bs�Br�Br�Br�Bq�Br�Bq�Bn�Bk�BjBiyBhsBffBdZBcTBbNBaHBbNBbNBaHBaHB`BB`BB`BB_;B^5BaHBcTBcTBe`BhsBk�Bk�BiyBiyBjBjBjBl�Bm�Bm�Bp�Bx�B}�B{�B�B�B}�B|�Bz�Bs�Bp�Bz�B�B~�B�B�B�B�B�B�B�B�%B�1B�DB�JB�VB�bB�bB�bB�oB�{B�uB�{B��B��B��B��B��B��B��B�B�9B�RB�jB�wB��BƨBǮBɺBɺB��B��B��B��B��B��B��B��B�B�
B�B�BB�TB�ZB�fB�sB�B�B�B�B�B�B�B�B�B��B��B	B	PB	VB	bB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	(�B	(�B	.B	33B	7LB	<jB	?}B	B�B	E�B	G�B	H�B	H�B	I�B	J�B	L�B	O�B	R�B	XB	]/B	^5B	]/B	[#B	YB	XB	W
B	VB	W
B	W
B	XB	XB	XB	ZB	ZB	ZB	ZB	ZB	_;B	dZB	dZB	e`B	ffB	jB	n�B	q�B	s�B	v�B	u�B	r�B	o�B	m�B	l�B	k�B	k�B	l�B	l�B	n�B	t�B	y�B	|�B	� B	�+B	�DB	�VB	�\B	�\B	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�B	�B	�'B	�-B	�3B	�?B	�?B	�?B	�RB	�XB	�^B	�dB	�jB	�jB	�jB	�jB	�wB	��B	��B	��B	��B	B	ĜB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�BB	�HB	�ZB	�fB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140851                              AO  ARCAADJP                                                                    20181024140851    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140851  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140851  QCF$                G�O�G�O�G�O�0               