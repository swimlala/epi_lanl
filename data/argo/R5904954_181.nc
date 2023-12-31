CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:30Z creation      
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
resolution        =���   axis      Z        <  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     <  B|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     <  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  R�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  [�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  c   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  d�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  sT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  u$   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  |`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     <  ~0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �,   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �D   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20181005191730  20181005191730  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @�ܤ�Jg�1   @�ܥWM@6]/��w�d�C��%1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  @���A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�33B�33B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C�fC  C�C  C
  C  C�fC  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx�Cz�C|  C~  C��C�  C�  C�  C��3C�  C��3C�  C��C�  C�  C��C�  C��C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C��C��3C�  C�  C�  C��C�  C��3C��3C��3C��3C�  C��C�  C��C�  C�  C��C��C��C�  C��C��C��C�  C�  C��C��C��C��C�  C��3C��3C�  C�  C�  C��C�  C��3C�  C��C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C��3C�  C��C��C��3C�  C��C��C�  C��C��C�  C�  C�  C��C��C�  C�  C�  C��C�  C��3C��3C�  C��C��C��D   D y�D  D�fDfD� D��D� D  D� D  D� D  Dy�D��Dy�D��Dy�D	  D	�fD
fD
�fDfD� D  D� D  D� D  D�fDfD� D  Dy�D  D�fDfD�fD  D�fD  Dy�D��D� DfD� D  D� D��D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"y�D#  D#� D$fD$� D$��D%y�D%��D&� D'  D'y�D'��D(y�D)  D)�fD*  D*� D+  D+� D+��D,y�D-  D-� D-��D.� D.��D/y�D0  D0� D1  D1�fD2  D2� D3  D3�fD4fD4� D4��D5� D6  D6y�D6��D7� D8  D8� D9  D9� D9��D:� D;fD;� D<  D<�fD=  D=� D>  D>� D>��D?y�D?��D@� DA  DA� DA��DBy�DC  DC�fDDfDD� DD��DEy�DE��DFy�DG  DG� DH  DH� DI  DI�fDJfDJ�fDKfDK�fDLfDL� DL��DMy�DM��DN� DN��DOy�DP  DP� DQ  DQ� DQ��DR� DS  DS� DTfDT� DT��DU�fDVfDV� DV�3DW� DXfDX�fDYfDY�fDZfDZ� D[fD[�fD\fD\�fD]fD]�fD^fD^�fD_fD_y�D`  D`y�D`��Day�Db  Db� DcfDc�fDd  Dd� De  De� Df  Dy�\D�R�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @=p�@��@��@��RA ��A@��A`��A�z�A�G�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh=qBp=qBx��B�Q�B�Q�B�Q�B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��C \C��C\C(�C\C
\C\C��C\C\C\C\C\C\C\C��C \C"\C$\C&\C(\C*\C+��C.\C0\C2\C4\C6\C8\C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C_��Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Cs��Cv\Cx(�Cz(�C|\C~\C�{C��C��C��C���C��C���C��C�{C��C��C�{C��C�{C��C��C�{C��C��C�{C��C���C��C��C��C��C�{C��C�{C��C��C��C��C���C��C��C��C��C���C���C�{C���C��C��C��C�{C��C���C���C���C���C��C�{C��C�{C��C��C�{C�{C�!HC��C�{C�!HC�!HC��C��C�{C�{C�{C�{C��C���C���C��C��C��C�{C��C���C��C�{C��C��C��C��C��C�{C��C��C�{C��C��C��C��C��C��C�{C��C���C��C��C���C��C�{C�{C���C��C�{C�{C��C�{C�{C��C��C��C�{C�{C��C��C��C�{C��C���C���C��C�{C�{C�{D �D }qD�D�=D
=D��D�qD��D�D��D�D��D�D}qD�qD}qD�qD}qD	�D	�=D

=D
�=D
=D��D�D��D�D��D�D�=D
=D��D�D}qD�D�=D
=D�=D�D�=D�D}qD�qD��D
=D��D�D��D�qD��D�D��D
=D��D�D��D�D��D�D��D�D��D�D��D �D �=D!�D!��D"�D"}qD#�D#��D$
=D$��D$�qD%}qD%�qD&��D'�D'}qD'�qD(}qD)�D)�=D*�D*��D+�D+��D+�qD,}qD-�D-��D-�qD.��D.�qD/}qD0�D0��D1�D1�=D2�D2��D3�D3�=D4
=D4��D4�qD5��D6�D6}qD6�qD7��D8�D8��D9�D9��D9�qD:��D;
=D;��D<�D<�=D=�D=��D>�D>��D>�qD?}qD?�qD@��DA�DA��DA�qDB}qDC�DC�=DD
=DD��DD�qDE}qDE�qDF}qDG�DG��DH�DH��DI�DI�=DJ
=DJ�=DK
=DK�=DL
=DL��DL�qDM}qDM�qDN��DN�qDO}qDP�DP��DQ�DQ��DQ�qDR��DS�DS��DT
=DT��DT�qDU�=DV
=DV��DV�
DW��DX
=DX�=DY
=DY�=DZ
=DZ��D[
=D[�=D\
=D\�=D]
=D]�=D^
=D^�=D_
=D_}qD`�D`}qD`�qDa}qDb�Db��Dc
=Dc�=Dd�Dd��De�De��Df�Dy�3D�T�D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�ĜA���A���A���A���A���A���A���A���A���A��
A��A��#A��#A��#A��#A��#A��/A��/A��;A���A�ȴA���A�ȴA���A���A���A�ȴA���A���A�ȴA���A�ȴAͺ^A͟�A͓uAͅA�z�A�t�A�dZA�5?A��A�$�A�t�A�VA�+A�&�A�r�A�M�A�JA�(�AɅA�bA�S�A�+Aİ!A�x�A�+A��A�=qA�=qA�r�A���A��7A���A���A���A�~�A�9XA��wA�ZA��A���A�7LA�O�A�A�jA�ƨA��jA�ZA��TA���A���A��TA�A��mA��A�?}A��A�ƨA�ffA��yA���A��A��;A��A�=qA�;dA�t�A���A��+A���A��A��
A�r�A�G�A��uA�p�A�\)A�C�A�oA�v�A�A���A��A�|�A�S�A��#A�ƨA��A��A�dZA���A� �A�S�A�oA��A�ZA�&�A��A���A��A��jA�$�A}|�Az�/Ayp�Aw?}Av��Av�AvȴAuAo�Am��Aln�Ak��Akl�Ajr�AhĜAfAe
=AcG�A`�DA]�A\�jA[
=AX~�AWVAS�ARZAQ�wAQt�AQ\)AQAP�AO��AM�AM��AL�\AK�
AK��AK�hAK`BAJJAHA�AFĜAEK�AD=qAB=qA?p�A=C�A:��A9VA8  A7ƨA77LA6n�A5��A5?}A4�`A3�;A1dZA/��A.�A,��A*�A)A)�wA)��A)
=A'��A&VA#oA!�A��AoA��AC�A��A��A�A1A��A?}A~�A�A�`A��A�A��AZA�AA��AC�A{A�AK�A��A
jA	?}A��Av�A�A7LA �AoA  A��AffA7L@�ff@�%@�(�@�o@�&�@�b@�  @�^@�b@�"�@���@���@�D@���@�=q@�Ĝ@��
@�-@��@�S�@�-@��@�|�@ޟ�@���@��#@��`@�A�@�dZ@ָR@և+@�=q@��@�X@���@�Q�@��;@ӍP@��H@�E�@�O�@�I�@��@�ȴ@͑h@�b@�C�@�C�@�\)@�@���@�V@�%@�l�@��@���@Å@�@���@���@�\)@�^5@��@���@��R@�-@�x�@�  @���@��#@��h@�p�@�b@�
=@�$�@�`B@�bN@�\)@�
=@�n�@�^5@�^5@�ff@��\@�G�@��F@�o@�K�@���@�1@��@��\@�@�/@���@�V@�x�@��@�z�@�1@�1@�(�@�I�@�1@��@��P@�l�@�33@�ȴ@���@�ff@���@�%@��j@�z�@�A�@�(�@�b@��@�  @��@�dZ@�"�@��@�+@�o@���@���@��y@�S�@�@�+@��@�@��y@���@��#@��@�?}@��@��D@�1'@�9X@�b@��;@���@�+@��@���@�7L@��`@��@�bN@�I�@��F@�K�@�@�@�
=@���@�^5@��@��-@���@���@��7@�O�@��@�r�@�Q�@�9X@�A�@� �@�1@���@�\)@�dZ@�dZ@�dZ@�\)@�dZ@��F@�1@�A�@��@�j@��@��D@��;@�o@��R@�v�@��R@�-@�?}@�z�@�  @���@�ƨ@��w@���@��P@��F@���@��@�|�@�S�@�o@��@��y@��@�~�@��@��T@���@���@���@�x�@��@zh
@l��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A�ĜA���A���A���A���A���A���A���A���A���A��
A��A��#A��#A��#A��#A��#A��/A��/A��;A���A�ȴA���A�ȴA���A���A���A�ȴA���A���A�ȴA���A�ȴAͺ^A͟�A͓uAͅA�z�A�t�A�dZA�5?A��A�$�A�t�A�VA�+A�&�A�r�A�M�A�JA�(�AɅA�bA�S�A�+Aİ!A�x�A�+A��A�=qA�=qA�r�A���A��7A���A���A���A�~�A�9XA��wA�ZA��A���A�7LA�O�A�A�jA�ƨA��jA�ZA��TA���A���A��TA�A��mA��A�?}A��A�ƨA�ffA��yA���A��A��;A��A�=qA�;dA�t�A���A��+A���A��A��
A�r�A�G�A��uA�p�A�\)A�C�A�oA�v�A�A���A��A�|�A�S�A��#A�ƨA��A��A�dZA���A� �A�S�A�oA��A�ZA�&�A��A���A��A��jA�$�A}|�Az�/Ayp�Aw?}Av��Av�AvȴAuAo�Am��Aln�Ak��Akl�Ajr�AhĜAfAe
=AcG�A`�DA]�A\�jA[
=AX~�AWVAS�ARZAQ�wAQt�AQ\)AQAP�AO��AM�AM��AL�\AK�
AK��AK�hAK`BAJJAHA�AFĜAEK�AD=qAB=qA?p�A=C�A:��A9VA8  A7ƨA77LA6n�A5��A5?}A4�`A3�;A1dZA/��A.�A,��A*�A)A)�wA)��A)
=A'��A&VA#oA!�A��AoA��AC�A��A��A�A1A��A?}A~�A�A�`A��A�A��AZA�AA��AC�A{A�AK�A��A
jA	?}A��Av�A�A7LA �AoA  A��AffA7L@�ff@�%@�(�@�o@�&�@�b@�  @�^@�b@�"�@���@���@�D@���@�=q@�Ĝ@��
@�-@��@�S�@�-@��@�|�@ޟ�@���@��#@��`@�A�@�dZ@ָR@և+@�=q@��@�X@���@�Q�@��;@ӍP@��H@�E�@�O�@�I�@��@�ȴ@͑h@�b@�C�@�C�@�\)@�@���@�V@�%@�l�@��@���@Å@�@���@���@�\)@�^5@��@���@��R@�-@�x�@�  @���@��#@��h@�p�@�b@�
=@�$�@�`B@�bN@�\)@�
=@�n�@�^5@�^5@�ff@��\@�G�@��F@�o@�K�@���@�1@��@��\@�@�/@���@�V@�x�@��@�z�@�1@�1@�(�@�I�@�1@��@��P@�l�@�33@�ȴ@���@�ff@���@�%@��j@�z�@�A�@�(�@�b@��@�  @��@�dZ@�"�@��@�+@�o@���@���@��y@�S�@�@�+@��@�@��y@���@��#@��@�?}@��@��D@�1'@�9X@�b@��;@���@�+@��@���@�7L@��`@��@�bN@�I�@��F@�K�@�@�@�
=@���@�^5@��@��-@���@���@��7@�O�@��@�r�@�Q�@�9X@�A�@� �@�1@���@�\)@�dZ@�dZ@�dZ@�\)@�dZ@��F@�1@�A�@��@�j@��@��D@��;@�o@��R@�v�@��R@�-@�?}@�z�@�  @���@�ƨ@��w@���@��P@��F@���@��@�|�@�S�@�o@��@��y@��@�~�@��@��T@���@���@���@�x�@��@zh
@l��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B{B{B{B{B{B{B{B{B{B�B�B{B{B{B{B{B�B�B�B�B{BuBoBoBoBoBoBoBhBoBoBhBhBhBbBbB\B\B\B\BbBoB�B�B1'B33B33B2-B/B,B,B)�B$�B�B�B!�B%�B(�B-B1'B49B1'B2-B6FB@�BM�B`BBo�Bz�B�uB��B�'B�}B��BǮB�RB�!B��B��B�uB�oB�bB�VB�+B�B�B|�B}�B{�Bx�Bw�Bk�B_;BVBS�BO�BK�BD�B9XB0!B&�B�B�BoBJB  B�B�NB�BB�5B�B��BƨB��B�{Bw�BjBZBR�BO�BD�B33B�B+B
�B
�B
�sB
�5B
ɺB
�B
��B
��B
��B
�hB
�1B
v�B
_;B
XB
I�B
G�B
E�B
B�B
6FB
�B
B	��B	��B	�B	�B	�;B	��B	ĜB	�RB	��B	��B	�VB	�B	v�B	k�B	bNB	_;B	]/B	\)B	\)B	[#B	ZB	S�B	J�B	H�B	C�B	?}B	>wB	=qB	;dB	49B	+B	"�B	�B	uB		7B��B�B�fB�;B�/B�)B�B�
B��B��B��B��BǮB�wB�LB�9B�?B�3B�-B�'B�B��B��B��B�uB�PB�DB�1B�+B�B�B�B~�B|�B{�By�Bw�Bt�Bq�Bn�BjBiyBhsBhsBgmBe`BbNB^5BZBT�BQ�BQ�BXBXBW
BYB[#BYBS�BT�B[#BQ�BA�BA�BD�BE�BL�BR�BM�BK�BL�BN�BN�BN�BO�BO�BS�BS�BQ�BP�BN�BR�BS�BS�BT�BT�BW
BXBYBYBYBYBYBYBXBXBYBYBXBXBYBZB[#B\)B^5BbNBaHBdZBgmBgmBiyBr�Bt�Bv�Bz�B� B� B~�B|�B{�By�Bx�Bu�Bs�Bq�Bo�Bm�Bl�Bk�Bo�Bt�Bx�Bx�Bx�Bz�Bz�B{�B}�B�%B�DB�hB��B��B��B��B��B��B��B��B�B�FB�qBǮBȴBǮB��B�B�B�BB�ZB�ZB�mB�B�B��B��B��B��B��B	  B	B	B	B	B	DB	PB	VB	bB	hB	hB	hB	oB	�B	�B	�B	�B	�B	�B	!�B	"�B	'�B	.B	5?B	8RB	;dB	;dB	<jB	C�B	H�B	H�B	H�B	H�B	H�B	L�B	R�B	T�B	VB	XB	]/B	`BB	aHB	dZB	gmB	hsB	jB	n�B	m�B	n�B	n�B	p�B	q�B	t�B	u�B	y�B	|�B	}�B	~�B	~�B	~�B	� B	�B	�B	�B	�PB	�\B	�\B	�\B	�hB	�hB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	�B	�FB	�?B	�-B	�B	�B	�B	�!B	�-B	�!B	�B	�!B	�'B	�3B	�?B	�?B	�9B	�LB	�qB	�}B	�}B	��B	��B	��B	��B	B	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B
BB
2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B{B{B{B{B{B{B{B{B{B�B�B{B{B{B{B{B�B�B�B�B{BuBoBoBoBoBoBoBhBoBoBhBhBhBbBbB\B\B\B\BbBoB�B�B1'B33B33B2-B/B,B,B)�B$�B�B�B!�B%�B(�B-B1'B49B1'B2-B6FB@�BM�B`BBo�Bz�B�uB��B�'B�}B��BǮB�RB�!B��B��B�uB�oB�bB�VB�+B�B�B|�B}�B{�Bx�Bw�Bk�B_;BVBS�BO�BK�BD�B9XB0!B&�B�B�BoBJB  B�B�NB�BB�5B�B��BƨB��B�{Bw�BjBZBR�BO�BD�B33B�B+B
�B
�B
�sB
�5B
ɺB
�B
��B
��B
��B
�hB
�1B
v�B
_;B
XB
I�B
G�B
E�B
B�B
6FB
�B
B	��B	��B	�B	�B	�;B	��B	ĜB	�RB	��B	��B	�VB	�B	v�B	k�B	bNB	_;B	]/B	\)B	\)B	[#B	ZB	S�B	J�B	H�B	C�B	?}B	>wB	=qB	;dB	49B	+B	"�B	�B	uB		7B��B�B�fB�;B�/B�)B�B�
B��B��B��B��BǮB�wB�LB�9B�?B�3B�-B�'B�B��B��B��B�uB�PB�DB�1B�+B�B�B�B~�B|�B{�By�Bw�Bt�Bq�Bn�BjBiyBhsBhsBgmBe`BbNB^5BZBT�BQ�BQ�BXBXBW
BYB[#BYBS�BT�B[#BQ�BA�BA�BD�BE�BL�BR�BM�BK�BL�BN�BN�BN�BO�BO�BS�BS�BQ�BP�BN�BR�BS�BS�BT�BT�BW
BXBYBYBYBYBYBYBXBXBYBYBXBXBYBZB[#B\)B^5BbNBaHBdZBgmBgmBiyBr�Bt�Bv�Bz�B� B� B~�B|�B{�By�Bx�Bu�Bs�Bq�Bo�Bm�Bl�Bk�Bo�Bt�Bx�Bx�Bx�Bz�Bz�B{�B}�B�%B�DB�hB��B��B��B��B��B��B��B��B�B�FB�qBǮBȴBǮB��B�B�B�BB�ZB�ZB�mB�B�B��B��B��B��B��B	  B	B	B	B	B	DB	PB	VB	bB	hB	hB	hB	oB	�B	�B	�B	�B	�B	�B	!�B	"�B	'�B	.B	5?B	8RB	;dB	;dB	<jB	C�B	H�B	H�B	H�B	H�B	H�B	L�B	R�B	T�B	VB	XB	]/B	`BB	aHB	dZB	gmB	hsB	jB	n�B	m�B	n�B	n�B	p�B	q�B	t�B	u�B	y�B	|�B	}�B	~�B	~�B	~�B	� B	�B	�B	�B	�PB	�\B	�\B	�\B	�hB	�hB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	�B	�FB	�?B	�-B	�B	�B	�B	�!B	�-B	�!B	�B	�!B	�'B	�3B	�?B	�?B	�9B	�LB	�qB	�}B	�}B	��B	��B	��B	��B	B	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B
BB
2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.06 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191730                              AO  ARCAADJP                                                                    20181005191730    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191730  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191730  QCF$                G�O�G�O�G�O�8000            