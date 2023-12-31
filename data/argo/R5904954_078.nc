CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:06Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191706  20181005191706  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               NA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @����Qm1   @��� <��@5
~��"��d)�7Kƨ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      NA   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB ��B'��B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B���B�  B�  B�  B�  B���B���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C1�fC4  C6�C8  C:  C<  C>  C?�fCB  CD  CE�fCH  CJ  CK�fCN  CP  CR  CT�CU�fCX  CZ  C\  C^  C`  Ca�fCd�Cf�Ch  Ci�fCk�fCm�fCp  Cr  Cs�fCu�fCw��Cy�fC|  C~  C�fC�  C�  C�  C��3C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C��C��3C�  C�  C�  C�  C��C��C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��fC��3C�  C��C�  C��3C�  C��3C��C��C��C�  C�  C��3C�  C��3C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C�  C��3C�  C��C��C�  C��3C��C�  C��3C��3C��3C�  C�  C�  C��C�  C��3C��fC��3C��3C�  C�  C��3C��3C��3D   D � D  Dy�D  D� D  D� D  D�fDfD�fD  D�fD�D�fD  Dy�D	  D	�fD
fD
y�D
��D� DfD� D  D�fDfD� D��Dy�D  Dy�D��D� D  D� D  D�fD�D�fDfD� D  D�fD  D� DfD� D  Dy�D  D�fDfD�fD  Dy�D  D�fD  D� D  Dy�D��D y�D!  D!� D"fD"� D#  D#�fD$  D$y�D$��D%�fD&fD&�fD'fD'� D(  D(� D(��D)y�D*  D*� D+fD+� D,  D,�fD,��D-y�D.  D.y�D.��D/� D0fD0� D1  D1� D1��D2y�D3fD3� D4  D4�fD5fD5� D6  D6y�D6��D7�fD8  D8y�D9  D9� D:  D:� D;  D;y�D;��D<y�D=  D=� D=��D>� D?fD?� D@  D@� DA  DAy�DB  DB�fDC  DC� DDfDD� DE  DE� DE��DF� DGfDG� DHfDH� DH��DIy�DJ  DJ� DK  DKy�DK�3DLy�DL��DM� DN  DNy�DOfDO�fDPfDP�fDP��DQ� DRfDR� DS  DS�fDT  DT� DUfDU� DV  DV� DW  DW�fDXfDX�fDY  DY� DZfDZ� DZ��D[� D\fD\� D]  D]� D^  D^�fD_  D_y�D_��D`� Da  Day�Da��Dby�Db��Dcy�Dc��Dd� De  De� Df  Df� DgfDg�fDg��Dhy�Dh��Diy�Di�3Djy�Dj��Dks3Dk��Dl�fDm  Dmy�DnfDn�fDofDo� Dp  Dpy�Dp��Dq� DrfDr�fDs�Ds�fDs��Dts3Dt��Du� DvfDv� DwfDw` Dy��D�0�D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��\@�\)A�A#�AC�Ac�A��
A��
A��
A��
A��
A��
A��
A��
B �B�B�BQ�B!�RB(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�B�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B���B�u�B�B�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�B�B�u�B�u�B�B�B�u�B�u�B�u�B�u�B�B�B�B�C :�C:�C:�C:�C:�C
:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C:�C :�C":�C$:�C&:�C(:�C*:�C,T{C.:�C0:�C2!GC4:�C6T{C8:�C::�C<:�C>:�C@!GCB:�CD:�CF!GCH:�CJ:�CL!GCN:�CP:�CR:�CTT{CV!GCX:�CZ:�C\:�C^:�C`:�Cb!GCdT{CfT{Ch:�Cj!GCl!GCn!GCp:�Cr:�Ct!GCv!GCx�Cz!GC|:�C~:�C��C�qC�qC�qC��C��C�qC�*>C�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�qC�qC�*>C�qC�*>C��C�qC�qC�qC�qC�*>C�*>C�qC�*>C�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC��C��C�qC�*>C�qC��C�qC��C�*>C�*>C�7C�qC�qC��C�qC��C�qC�*>C�qC�qC�qC�*>C�*>C�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�qC�*>C�qC�qC�qC��C�qC�qC�qC��C�qC�qC��C�qC�*>C�*>C�qC�qC�qC�qC�qC�qC��C�qC�*>C�*>C�qC��C�*>C�qC��C��C��C�qC�qC�qC�*>C�qC��C��C��C��C�qC�qC��C��C��D �D ��D�D�RD�D��D�D��D�D�DD�D�D�D�D�D�D�RD	�D	�D
D
�RDRD��DD��D�D�DD��DRD�RD�D�RDRD��D�D��D�D�D�D�DD��D�D�D�D��DD��D�D�RD�D�DD�D�D�RD�D�D�D��D�D�RD RD �RD!�D!��D"D"��D#�D#�D$�D$�RD%RD%�D&D&�D'D'��D(�D(��D)RD)�RD*�D*��D+D+��D,�D,�D-RD-�RD.�D.�RD/RD/��D0D0��D1�D1��D2RD2�RD3D3��D4�D4�D5D5��D6�D6�RD7RD7�D8�D8�RD9�D9��D:�D:��D;�D;�RD<RD<�RD=�D=��D>RD>��D?D?��D@�D@��DA�DA�RDB�DB�DC�DC��DDDD��DE�DE��DFRDF��DGDG��DHDH��DIRDI�RDJ�DJ��DK�DK�RDL�DL�RDMRDM��DN�DN�RDODO�DPDP�DQRDQ��DRDR��DS�DS�DT�DT��DUDU��DV�DV��DW�DW�DXDX�DY�DY��DZDZ��D[RD[��D\D\��D]�D]��D^�D^�D_�D_�RD`RD`��Da�Da�RDbRDb�RDcRDc�RDdRDd��De�De��Df�Df��DgDg�DhRDh�RDiRDi�RDj�Dj�RDkRDk��DlRDl�Dm�Dm�RDnDn�DoDo��Dp�Dp�RDqRDq��DrDr�Ds�Ds�DtRDt��DuRDu��DvDv��DwDwn�Dy��D�8RD�Ф11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AڸRAڼjAڸRA�ĜA�ƨA���A��
A��
A��A��A��A��#A��A��#A��#A��#A��/A�A�A��A�ȴA�jA�bA�{A�1A�
=A��A��mAȅA�/A��
A�jA���A×�A�v�A�1'A�ȴA��#A���A�ƨA��A���A�=qA��A�M�A��A�l�A�9XA�I�A���A�K�A�Q�A��7A�/A�oA�;dA� �A��A��+A�l�A��A�ƨA�I�A�  A��+A�VA��
A�`BA���A���A��A���A�^5A���A�+A��A��RA���A���A�`BA�9XA��jA���A��A���A�(�A�x�A��A�I�A��A��#A�{A��PA��wA��A��#A�5?A���A~��A|�/A|z�A{C�Aw��As�^ApȴAot�An�RAl�RAjz�Ah^5AgS�Ae�7Ac��Aax�A\�AZ�/AWp�AT1ARbNAQ;dAP�yAOK�ALĜAKO�AIhsAGp�ABffA@�jA?A>z�A<M�A;��A:�A:  A8�jA8Q�A81A4r�A1G�A/��A/7LA/oA.��A.�A-33A,E�A+p�A(�+A'�A%�wA$��A#%A"�9A!��A�#A�7AhsA7LA��AVA(�A��A��AA�A�TA\)A��A9XA�PAbNAt�A-Av�A&�AbNA�wA��A��AoAA
=qA	�PAE�A�FAhsAbNAK�A~�A�A
=A~�A �A�hA ��@��!@��\@�V@�E�@��@��@���@�-@�;d@��^@�hs@��j@��@� �@�S�@�%@��
@���@�E�@�@�p�@�9@�w@�v�@��`@�I�@�t�@�%@�
=@ޗ�@�$�@�%@��
@��y@�7L@�V@�Q�@�Q�@�1@�M�@�Q�@˶F@ʸR@�K�@ƸR@�v�@��T@ź^@��@ċD@�|�@\@��@��h@��@�p�@�X@�X@���@���@�O�@�Ĝ@��@��u@�1'@��;@�1@� �@�t�@��y@�;d@��P@��@���@��P@�t�@��@�$�@��-@�X@�Ĝ@���@�@�M�@��-@���@���@���@�X@�G�@��h@�A�@��@��@���@�p�@�/@��`@�j@��m@�"�@�9X@�33@�-@�x�@�X@��@��j@��@�Ĝ@�Q�@���@�;d@�dZ@�dZ@�v�@�p�@�?}@�7L@�?}@���@��@��;@��j@���@�G�@�dZ@��`@�b@� �@� �@�dZ@�"�@��R@�5?@��T@��7@�z�@��@�@��@�"�@�
=@�o@���@��/@�Q�@���@�ƨ@��w@�dZ@�33@��F@��j@���@�j@� �@��;@��@���@�  @��P@�+@�$�@��@��@��@��@�@��^@��-@��-@�p�@�O�@�?}@�7L@��@��u@���@�"�@�;d@���@��H@��@�ȴ@��!@��H@��!@�M�@�-@��@�?}@��@�z�@��@�r�@�Q�@�b@��;@�\)@��@��+@�M�@�J@�x�@�?}@�%@��9@��u@��@�A�@���@���@�|�@�S�@�"�@��@�o@�@��@���@���@�^5@�5?@��@���@��T@�@���@�?}@��@��@��@�Z@�9X@��;@���@�dZ@�;d@��H@���@�n�@�{@���@�Ĝ@�z�@�r�@�1@���@���@�|�@�o@���@���@�~�@�J@�$�@�$�@���@��@��-@��@�p�@�&�@���@��D@�bN@�bN@�j@�j@�j@�j@�Q�@� �@�  @���@�;d@���@��H@���@�-@��^@��h@�x�@�x�@�p�@�p�@�hs@�`B@�X@�G�@�G�@��/@�Q�@��;@�C�@�
=@��@��\@�5?@�d�@s�6@jJ�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AڸRAڼjAڸRA�ĜA�ƨA���A��
A��
A��A��A��A��#A��A��#A��#A��#A��/A�A�A��A�ȴA�jA�bA�{A�1A�
=A��A��mAȅA�/A��
A�jA���A×�A�v�A�1'A�ȴA��#A���A�ƨA��A���A�=qA��A�M�A��A�l�A�9XA�I�A���A�K�A�Q�A��7A�/A�oA�;dA� �A��A��+A�l�A��A�ƨA�I�A�  A��+A�VA��
A�`BA���A���A��A���A�^5A���A�+A��A��RA���A���A�`BA�9XA��jA���A��A���A�(�A�x�A��A�I�A��A��#A�{A��PA��wA��A��#A�5?A���A~��A|�/A|z�A{C�Aw��As�^ApȴAot�An�RAl�RAjz�Ah^5AgS�Ae�7Ac��Aax�A\�AZ�/AWp�AT1ARbNAQ;dAP�yAOK�ALĜAKO�AIhsAGp�ABffA@�jA?A>z�A<M�A;��A:�A:  A8�jA8Q�A81A4r�A1G�A/��A/7LA/oA.��A.�A-33A,E�A+p�A(�+A'�A%�wA$��A#%A"�9A!��A�#A�7AhsA7LA��AVA(�A��A��AA�A�TA\)A��A9XA�PAbNAt�A-Av�A&�AbNA�wA��A��AoAA
=qA	�PAE�A�FAhsAbNAK�A~�A�A
=A~�A �A�hA ��@��!@��\@�V@�E�@��@��@���@�-@�;d@��^@�hs@��j@��@� �@�S�@�%@��
@���@�E�@�@�p�@�9@�w@�v�@��`@�I�@�t�@�%@�
=@ޗ�@�$�@�%@��
@��y@�7L@�V@�Q�@�Q�@�1@�M�@�Q�@˶F@ʸR@�K�@ƸR@�v�@��T@ź^@��@ċD@�|�@\@��@��h@��@�p�@�X@�X@���@���@�O�@�Ĝ@��@��u@�1'@��;@�1@� �@�t�@��y@�;d@��P@��@���@��P@�t�@��@�$�@��-@�X@�Ĝ@���@�@�M�@��-@���@���@���@�X@�G�@��h@�A�@��@��@���@�p�@�/@��`@�j@��m@�"�@�9X@�33@�-@�x�@�X@��@��j@��@�Ĝ@�Q�@���@�;d@�dZ@�dZ@�v�@�p�@�?}@�7L@�?}@���@��@��;@��j@���@�G�@�dZ@��`@�b@� �@� �@�dZ@�"�@��R@�5?@��T@��7@�z�@��@�@��@�"�@�
=@�o@���@��/@�Q�@���@�ƨ@��w@�dZ@�33@��F@��j@���@�j@� �@��;@��@���@�  @��P@�+@�$�@��@��@��@��@�@��^@��-@��-@�p�@�O�@�?}@�7L@��@��u@���@�"�@�;d@���@��H@��@�ȴ@��!@��H@��!@�M�@�-@��@�?}@��@�z�@��@�r�@�Q�@�b@��;@�\)@��@��+@�M�@�J@�x�@�?}@�%@��9@��u@��@�A�@���@���@�|�@�S�@�"�@��@�o@�@��@���@���@�^5@�5?@��@���@��T@�@���@�?}@��@��@��@�Z@�9X@��;@���@�dZ@�;d@��H@���@�n�@�{@���@�Ĝ@�z�@�r�@�1@���@���@�|�@�o@���@���@�~�@�J@�$�@�$�@���@��@��-@��@�p�@�&�@���@��D@�bN@�bN@�j@�j@�j@�j@�Q�@� �@�  @���@�;d@���@��H@���@�-@��^@��h@�x�@�x�@�p�@�p�@�hs@�`B@�X@�G�@�G�@��/@�Q�@��;@�C�@�
=@��@��\@�5?@�d�@s�6@jJ�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�/B��B��B��B�B�HB�yB��B  BbBoB&�B7LB>wBM�B\)B^5BbNBz�B�1B�oB��B��B��B�B�B�!B�B�B�!B��B��B��B��B��B�{B�B~�By�Bs�B_;BK�BA�B;dB7LB0!B-B+B#�B�BB�HBB�B��B�VBu�BffB[#BVBR�BO�BG�B9XB$�B1B
��B
�NB
��B
ĜB
�qB
�B
��B
�PB
}�B
q�B
dZB
YB
Q�B
B�B
49B
0!B
'�B
\B	��B	�fB	�)B	�B	��B	B	�FB	�B	��B	�VB	~�B	cTB	VB	G�B	/B	&�B	�B	�B	{B	1B	B��B�B�5B�B��B��BĜB��B�}B�qB�jB�^B�LB�'B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�{B�uB�uB�hB�hB�VB�\B�VB�VB�VB�PB�DB�1B�B�B~�B~�B}�B{�Bz�B{�Bz�B|�B|�B{�B|�B{�B{�B|�B|�B{�Bz�Bw�Bs�Bs�Br�Bu�Bx�Bx�Bv�Bv�Bv�Bp�Be`B`BB^5B]/B\)BaHB_;B_;B_;B_;B_;B_;BbNBdZBgmBhsBhsBiyBjBk�Bu�Bw�B{�B|�B}�B}�B~�B�B�1B�JB�PB�bB��B��B��B��B�B�?B�LB�RB�XB�XB�XB�^B�qB�dB�jB�qBBBĜBŢB��B��B��B��BȴB��B�B�B�)B�/B�5B��B��B��B�#B�HB�ZB�`B�B�B�B�B�B��B��B��B��B��B	B	B	1B	
=B	PB	�B	�B	 �B	0!B	<jB	>wB	@�B	A�B	C�B	F�B	G�B	H�B	H�B	G�B	C�B	C�B	G�B	J�B	K�B	M�B	Q�B	YB	bNB	aHB	cTB	e`B	hsB	jB	l�B	q�B	x�B	{�B	z�B	{�B	|�B	|�B	}�B	�B	�B	�B	�%B	�7B	�=B	�JB	�PB	�PB	�PB	�PB	�VB	�VB	�\B	�\B	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�'B	�'B	�3B	�3B	�FB	�LB	�LB	�RB	�RB	�XB	�dB	�qB	��B	��B	��B	B	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�#B	�/B	�5B	�;B	�;B	�HB	�NB	�NB	�NB	�ZB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
1B

=B
DB
JB
PB
JB
PB
JB
{B
�B
 B
'22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�/B��B��B��B�B�HB�yB��B  BbBoB&�B7LB>wBM�B\)B^5BbNBz�B�1B�oB��B��B��B�B�B�!B�B�B�!B��B��B��B��B��B�{B�B~�By�Bs�B_;BK�BA�B;dB7LB0!B-B+B#�B�BB�HBB�B��B�VBu�BffB[#BVBR�BO�BG�B9XB$�B1B
��B
�NB
��B
ĜB
�qB
�B
��B
�PB
}�B
q�B
dZB
YB
Q�B
B�B
49B
0!B
'�B
\B	��B	�fB	�)B	�B	��B	B	�FB	�B	��B	�VB	~�B	cTB	VB	G�B	/B	&�B	�B	�B	{B	1B	B��B�B�5B�B��B��BĜB��B�}B�qB�jB�^B�LB�'B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�{B�uB�uB�hB�hB�VB�\B�VB�VB�VB�PB�DB�1B�B�B~�B~�B}�B{�Bz�B{�Bz�B|�B|�B{�B|�B{�B{�B|�B|�B{�Bz�Bw�Bs�Bs�Br�Bu�Bx�Bx�Bv�Bv�Bv�Bp�Be`B`BB^5B]/B\)BaHB_;B_;B_;B_;B_;B_;BbNBdZBgmBhsBhsBiyBjBk�Bu�Bw�B{�B|�B}�B}�B~�B�B�1B�JB�PB�bB��B��B��B��B�B�?B�LB�RB�XB�XB�XB�^B�qB�dB�jB�qBBBĜBŢB��B��B��B��BȴB��B�B�B�)B�/B�5B��B��B��B�#B�HB�ZB�`B�B�B�B�B�B��B��B��B��B��B	B	B	1B	
=B	PB	�B	�B	 �B	0!B	<jB	>wB	@�B	A�B	C�B	F�B	G�B	H�B	H�B	G�B	C�B	C�B	G�B	J�B	K�B	M�B	Q�B	YB	bNB	aHB	cTB	e`B	hsB	jB	l�B	q�B	x�B	{�B	z�B	{�B	|�B	|�B	}�B	�B	�B	�B	�%B	�7B	�=B	�JB	�PB	�PB	�PB	�PB	�VB	�VB	�\B	�\B	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�'B	�'B	�3B	�3B	�FB	�LB	�LB	�RB	�RB	�XB	�dB	�qB	��B	��B	��B	B	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�B	�B	�B	�#B	�#B	�/B	�5B	�;B	�;B	�HB	�NB	�NB	�NB	�ZB	�ZB	�`B	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
%B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
1B

=B
DB
JB
PB
JB
PB
JB
{B
�B
 B
'22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191706                              AO  ARCAADJP                                                                    20181005191706    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191706  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191706  QCF$                G�O�G�O�G�O�8000            