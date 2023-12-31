CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:16Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190616  20181005190616  5904953 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6432                            2B  A   APEX                            7467                            062512                          846 @ןY\5`1   @ןY��@3�     �c���+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   B   @���@�  A   A   A@  A`  A�  A�  A�33A�  A���A���A���A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�33B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B���B�  B�  B�33B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33C   C  C  C  C�C
  C  C�fC  C  C  C  C  C�C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C7�fC:  C<�C>�C@  CB  CD  CF  CH  CJ  CK�fCM�fCP  CR�CT�CU�fCW�fCZ�C\  C]�fC_��Ca��Cc��Ce�fCh�Cj  Ck�fCn  Cp  Cr  Cs�fCv  Cx�Cz33C|  C}�fC��C��fC��fC��3C�  C��C��C��C��fC��fC�  C��C��C��C��3C��3C��C��C��C��C��3C��fC��fC��C��C��fC��C��3C��C��3C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C��C��3C��C��3C��3C��C�  C��fC��3C��3C��3C��3C�  C��C��C��C��3C��C��C��fC�  C��C��3C�  C��C��3C��3C�  C��C��C��C��C��fC��fC��fC��3C��fC��fC��fC��fC��fC��fC��3C��3C��3C�  C�  C�  C��C�  C��3C��C��3D   D � D  D� D��D� D  D�fDfD�fD  D�fD  D� D  Dy�DfD�fD��D	s3D
fD
� DfD�fD  Dy�D��D� D��D� D  D� D  D� DfD�fD  Dy�D��D�fD  D� D  Ds3D��D�fD  Dy�DfDy�D��D� D  Dy�D��Dy�DfD�fD�3Ds3D  D�fD  Dy�D fD � D ��D!y�D!�3D"� D#  D#s3D$fD$� D%  D%� D&  D&� D&��D'�fD(�D(� D(��D)�fD*  D*�fD+fD+�fD+��D,y�D,��D-�fD.fD.y�D.��D/� D0  D0y�D1  D1�fD2fD2�fD3  D3y�D4  D4�fD5fD5�fD6  D6� D7fD7y�D8  D8� D8��D9� D:fD:�fD;fD;�fD<fD<� D=  D=�fD>fD>�fD?fD?�fD@  D@y�D@��DAy�DA��DBy�DB��DCy�DD  DD��DEfDE��DF�DF��DGfDG� DH  DH�fDIfDI�fDJfDJ� DK  DK�fDL�DL� DL��DM�fDN�DN��DO  DOy�DO�3DPs3DP��DQ�fDRfDR��DS�DS� DS�3DTs3DU  DU��DVfDV�fDWfDW� DW��DX� DY  DYy�DZ  DZ��D[fD[� D[��D\y�D]  D]s3D]��D^� D_  D_y�D_��D`� Da  Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDqfDq� Dr  Dr� DsfDs� Dt  Dt� Du  Du� Dv  Dv� Dv��Dw� Dx  Dy�=D�5111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�(�@�(�A{A>{A^{A~{A�
=A�=pA�
=A��
A��
A��
A�
=A�
=B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B�B���B�B�B�B�B��\B�B�B�B�B�B��\B�B�B���B���B���B���B�B�B�B�B�B�B�B�B�B���B���B�C�HC�HC�HC��C	�HC�HCǮC�HC�HC�HC�HC�HC��C�HCǮC�HC!�HC#�HC%�HC'�HC)�HC+�HC-�HC/��C1�HC3�HC5�HC7ǮC9�HC;��C=��C?�HCA�HCC�HCE�HCG�HCI�HCKǮCMǮCO�HCQ��CS��CUǮCWǮCY��C[�HC]ǮC_�Ca�Cc�CeǮCg��Ci�HCkǮCm�HCo�HCq�HCsǮCu�HCw��Cz{C{�HC}ǮC�C��
C��
C���C��C��qC��qC��qC��
C��
C��C��qC�
>C��qC���C���C��qC��qC�
>C�
>C���C��
C��
C��qC�
>C��
C�
>C���C��qC���C��C��qC��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��C��C��C��C��C��C��qC��C��C��C��C��C��C��qC��C��C��qC��C��C��qC���C�
>C���C���C�
>C��C��
C���C���C���C���C��C��qC�
>C��qC���C��qC��qC��
C��C�
>C���C��C�
>C���C���C��C��qC�
>C�
>C��qC��
C��
C��
C���C��
C��
C��
C��
C��
C��
C���C���C���C��C��C��C��qC��C���C��qC���C��D xRD �RDxRD��DxRD�RD~�D��D~�D�RD~�D�RDxRD�RDq�D��D~�D��D	k�D	��D
xRD
��D~�D�RDq�D��DxRD��DxRD�RDxRD�RDxRD��D~�D�RDq�D��D~�D�RDxRD�RDk�D��D~�D�RDq�D��Dq�D��DxRD�RDq�D��Dq�D��D~�D�Dk�D�RD~�D�RDq�D��D xRD ��D!q�D!�D"xRD"�RD#k�D#��D$xRD$�RD%xRD%�RD&xRD&��D'~�D(D(xRD(��D)~�D)�RD*~�D*��D+~�D+��D,q�D,��D-~�D-��D.q�D.��D/xRD/�RD0q�D0�RD1~�D1��D2~�D2�RD3q�D3�RD4~�D4��D5~�D5�RD6xRD6��D7q�D7�RD8xRD8��D9xRD9��D:~�D:��D;~�D;��D<xRD<�RD=~�D=��D>~�D>��D?~�D?�RD@q�D@��DAq�DA��DBq�DB��DCq�DC�RDD�DD��DE�DFDF�DF��DGxRDG�RDH~�DH��DI~�DI��DJxRDJ�RDK~�DLDLxRDL��DM~�DNDN�DN�RDOq�DO�DPk�DP��DQ~�DQ��DR�DSDSxRDS�DTk�DT�RDU�DU��DV~�DV��DWxRDW��DXxRDX�RDYq�DY�RDZ�DZ��D[xRD[��D\q�D\�RD]k�D]��D^xRD^�RD_q�D_��D`xRD`�RDl�RDmxRDm�RDnxRDn�RDoxRDo�RDp~�Dp��DqxRDq�RDrxRDr��DsxRDs�RDtxRDt�RDuxRDu�RDvxRDv��DwxRDw�RDy��D�1H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�t�A�z�A�x�A�$�Aŕ�A�v�A�jA�Q�A�(�A�&�A�{A�1A�%A�JA�
=A�%A�oA��A�/A�7LA�;dA�;dA�7LA�7LA�7LA�1'A�1'A�+A�"�A��A�JA�A�1A�JA�bA�{A�1A�A��A�XA�Q�A���A�t�AöFA��yA�^5A���A�E�A�~�A���A�bNA�VA�C�A�(�A�E�A���A�M�A��A��+A�x�A�ĜA��A�r�A��
A�ƨA��7A�hsA���A�ĜA��A��9A��PA���A�ƨA��HA��mA�bA��yA���A��HA���A��A�A�oA��uA��hA�ffA��A���A���A��9A�ĜA���A��A��wA���A�=qA��`A�jA��!A��wA��wA~~�A{�7Axn�Atv�Ar�uAq�PAk�Af�9Ad�DAbz�A`ZA^��AZ�AY�AXI�AW�AU�ATZAS�^AS?}AQdZAM�;AL�RAK��AJJAG�AD�HAA��A@(�A?��A?\)A?�A>��A>��A>z�A=�A:��A9oA6�`A6��A5�
A5;dA4$�A3K�A3VA2r�A0n�A.�HA.E�A-��A+�A*=qA(�jA(�A'%A%�-A$�`A#�mA"�uA!VA��Al�A�\A�yA��A��A��A�AXA��AI�A�mAx�A�A�\A{AƨA��A��A�A�mA�A�jA�PA
M�A�A  At�A^5AC�A��AĜA�A=qAO�A��A V@��@�V@�@�z�@�n�@�%@�|�@��@���@�Z@�+@�E�@�$�@�x�@�Ĝ@�Z@��@�F@�dZ@�R@�5?@�O�@��/@�9X@�K�@��@�j@�9X@�P@�E�@�V@���@�`B@ە�@���@�1'@׍P@�S�@�"�@��H@֧�@��@��`@�"�@�?}@�A�@���@���@Η�@�X@̃@�ƨ@ʰ!@ɲ-@�V@�|�@ư!@ź^@�1@�;d@�^5@�@�Q�@��@�K�@���@��@�G�@�/@��@���@��j@�r�@�Z@�(�@��F@�l�@�o@�5?@��7@��`@�z�@�A�@�  @��P@�S�@�o@��H@��y@��y@��H@���@���@�~�@�v�@�$�@��#@��^@��^@��@�V@���@��@��@�A�@��@�ȴ@�@��9@��@�Z@���@���@���@�\)@��@��@�(�@��@�33@�+@�+@�;d@�K�@�C�@���@�M�@�M�@�n�@�v�@�v�@�V@�E�@�=q@�5?@���@�bN@�z�@��j@�I�@���@�ff@�@���@��`@���@��u@��u@��@�bN@�;d@��\@�E�@�=q@��@���@��h@��7@�p�@��`@�1@��w@�l�@��@��R@�v�@�{@��^@���@�hs@�&�@��/@��u@�j@�(�@��@��@��@�l�@�
=@��@���@�n�@�E�@�-@��@�J@���@�p�@�7L@��/@�j@�b@�t�@�C�@�
=@�ȴ@�n�@�-@��#@�@�x�@�O�@���@�bN@��m@��@��
@�1@�  @��@���@��P@��P@��P@���@��P@�t�@�
=@�+@�@��\@�^5@��@��-@�`B@�?}@��@��/@��@�z�@�I�@��@�  @���@�t�@�C�@�"�@���@���@�5?@���@���@�?}@���@���@��@�(�@��;@��F@�{@���@�`B@�/@��@��@��@�j@�Q�@��m@�t�@�"�@�o@��@��@�ȴ@��+@�^5@�5?@��@���@��@�&�@��@r��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�t�A�z�A�x�A�$�Aŕ�A�v�A�jA�Q�A�(�A�&�A�{A�1A�%A�JA�
=A�%A�oA��A�/A�7LA�;dA�;dA�7LA�7LA�7LA�1'A�1'A�+A�"�A��A�JA�A�1A�JA�bA�{A�1A�A��A�XA�Q�A���A�t�AöFA��yA�^5A���A�E�A�~�A���A�bNA�VA�C�A�(�A�E�A���A�M�A��A��+A�x�A�ĜA��A�r�A��
A�ƨA��7A�hsA���A�ĜA��A��9A��PA���A�ƨA��HA��mA�bA��yA���A��HA���A��A�A�oA��uA��hA�ffA��A���A���A��9A�ĜA���A��A��wA���A�=qA��`A�jA��!A��wA��wA~~�A{�7Axn�Atv�Ar�uAq�PAk�Af�9Ad�DAbz�A`ZA^��AZ�AY�AXI�AW�AU�ATZAS�^AS?}AQdZAM�;AL�RAK��AJJAG�AD�HAA��A@(�A?��A?\)A?�A>��A>��A>z�A=�A:��A9oA6�`A6��A5�
A5;dA4$�A3K�A3VA2r�A0n�A.�HA.E�A-��A+�A*=qA(�jA(�A'%A%�-A$�`A#�mA"�uA!VA��Al�A�\A�yA��A��A��A�AXA��AI�A�mAx�A�A�\A{AƨA��A��A�A�mA�A�jA�PA
M�A�A  At�A^5AC�A��AĜA�A=qAO�A��A V@��@�V@�@�z�@�n�@�%@�|�@��@���@�Z@�+@�E�@�$�@�x�@�Ĝ@�Z@��@�F@�dZ@�R@�5?@�O�@��/@�9X@�K�@��@�j@�9X@�P@�E�@�V@���@�`B@ە�@���@�1'@׍P@�S�@�"�@��H@֧�@��@��`@�"�@�?}@�A�@���@���@Η�@�X@̃@�ƨ@ʰ!@ɲ-@�V@�|�@ư!@ź^@�1@�;d@�^5@�@�Q�@��@�K�@���@��@�G�@�/@��@���@��j@�r�@�Z@�(�@��F@�l�@�o@�5?@��7@��`@�z�@�A�@�  @��P@�S�@�o@��H@��y@��y@��H@���@���@�~�@�v�@�$�@��#@��^@��^@��@�V@���@��@��@�A�@��@�ȴ@�@��9@��@�Z@���@���@���@�\)@��@��@�(�@��@�33@�+@�+@�;d@�K�@�C�@���@�M�@�M�@�n�@�v�@�v�@�V@�E�@�=q@�5?@���@�bN@�z�@��j@�I�@���@�ff@�@���@��`@���@��u@��u@��@�bN@�;d@��\@�E�@�=q@��@���@��h@��7@�p�@��`@�1@��w@�l�@��@��R@�v�@�{@��^@���@�hs@�&�@��/@��u@�j@�(�@��@��@��@�l�@�
=@��@���@�n�@�E�@�-@��@�J@���@�p�@�7L@��/@�j@�b@�t�@�C�@�
=@�ȴ@�n�@�-@��#@�@�x�@�O�@���@�bN@��m@��@��
@�1@�  @��@���@��P@��P@��P@���@��P@�t�@�
=@�+@�@��\@�^5@��@��-@�`B@�?}@��@��/@��@�z�@�I�@��@�  @���@�t�@�C�@�"�@���@���@�5?@���@���@�?}@���@���@��@�(�@��;@��F@�{@���@�`B@�/@��@��@��@�j@�Q�@��m@�t�@�"�@�o@��@��@�ȴ@��+@�^5@�5?@��@���@��@�&�@��@r��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B|�B|�B|�B}�B�B�B�B�B�B�B�B�B�B�B�%B�+B�=B�PB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�XB��B��BVB�B(�B;dBP�B\)BdZBcTBF�B:^B>wBC�BK�BdZB� B� B��BȴB�FB�uB�BaHB<jB(�B:^B@�BD�B<jB\B��B�5B�B
=B2-BG�BT�B9XB#�B�B  B�B�BĜB�9B��B�PBbNBVBI�B<jB.BoB
�B
�`B
�#B
�qB
��B
~�B
k�B
]/B
H�B
49B
�B
B	�B	��B	�}B	�9B	�JB	jB	aHB	XB	K�B	@�B	/B	)�B	&�B	#�B	�B	�B	uB	\B	B��B�B�mB�5B��BƨB�dB�XB�RB�FB�?B�9B�3B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�uB�{B�{B�{B�{B�uB�uB�oB�hB�oB�oB�oB�\B�bB�hB�bB�\B�PB�=B�=B�DB�DB�=B�DB�=B�=B�7B�7B�7B�7B�+B�%B�+B�+B�PB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�3B�9B�?B�9B�?B�XB�RB�LB�?B�9B�?B�?B�FB�FB�FB�LB�dBǮBĜBĜBŢBŢBǮB��B��B��B��B��B��B�B�)B�;B�`B�mB�B�B�B�B��B��B	B	B	B	B	B	%B	%B	%B	+B	1B		7B	DB	uB	�B	�B	�B	�B	!�B	%�B	&�B	(�B	+B	+B	+B	,B	-B	-B	.B	/B	0!B	1'B	1'B	33B	8RB	:^B	;dB	;dB	;dB	;dB	<jB	@�B	B�B	C�B	D�B	F�B	F�B	P�B	ZB	[#B	S�B	R�B	Q�B	R�B	T�B	VB	W
B	XB	YB	[#B	^5B	bNB	ffB	gmB	k�B	n�B	p�B	q�B	q�B	q�B	r�B	t�B	x�B	|�B	}�B	� B	� B	�B	�%B	�1B	�=B	�JB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�?B	�?B	�FB	�RB	�RB	�jB	�}B	��B	ÖB	ĜB	ŢB	ŢB	ŢB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�BB	�BB	�HB	�HB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�`B	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��By�B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
%B
%B
%B
%B
1B
1B
	7B

=B
DB
DB
JB
�B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222  B|�B|�B|�B}�B�B�B�B�B�B�B�B�B�B�B�%B�+B�=B�PB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�XB��B��BVB�B(�B;dBP�B\)BdZBcTBF�B:^B>wBC�BK�BdZB� B� B��BȴB�FB�uB�BaHB<jB(�B:^B@�BD�B<jB\B��B�5B�B
=B2-BG�BT�B9XB#�B�B  B�B�BĜB�9B��B�PBbNBVBI�B<jB.BoB
�B
�`B
�#B
�qB
��B
~�B
k�B
]/B
H�B
49B
�B
B	�B	��B	�}B	�9B	�JB	jB	aHB	XB	K�B	@�B	/B	)�B	&�B	#�B	�B	�B	uB	\B	B��B�B�mB�5B��BƨB�dB�XB�RB�FB�?B�9B�3B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�uB�{B�{B�{B�{B�uB�uB�oB�hB�oB�oB�oB�\B�bB�hB�bB�\B�PB�=B�=B�DB�DB�=B�DB�=B�=B�7B�7B�7B�7B�+B�%B�+B�+B�PB�hB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�'B�3B�9B�?B�9B�?B�XB�RB�LB�?B�9B�?B�?B�FB�FB�FB�LB�dBǮBĜBĜBŢBŢBǮB��B��B��B��B��B��B�B�)B�;B�`B�mB�B�B�B�B��B��B	B	B	B	B	B	%B	%B	%B	+B	1B		7B	DB	uB	�B	�B	�B	�B	!�B	%�B	&�B	(�B	+B	+B	+B	,B	-B	-B	.B	/B	0!B	1'B	1'B	33B	8RB	:^B	;dB	;dB	;dB	;dB	<jB	@�B	B�B	C�B	D�B	F�B	F�B	P�B	ZB	[#B	S�B	R�B	Q�B	R�B	T�B	VB	W
B	XB	YB	[#B	^5B	bNB	ffB	gmB	k�B	n�B	p�B	q�B	q�B	q�B	r�B	t�B	x�B	|�B	}�B	� B	� B	�B	�%B	�1B	�=B	�JB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�?B	�?B	�FB	�RB	�RB	�jB	�}B	��B	ÖB	ĜB	ŢB	ŢB	ŢB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�BB	�BB	�HB	�HB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�`B	�`B	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��By�B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
%B
%B
%B
%B
1B
1B
	7B

=B
DB
DB
JB
�B
�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.12 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190616                              AO  ARCAADJP                                                                    20181005190616    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190616  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190616  QCF$                G�O�G�O�G�O�8000            