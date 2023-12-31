CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:36Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190536  20181005190536  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @���F�g1   @���q��@0��S����cޏ\(��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@���A   A   A>ffA^ffA~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B���B�  B�  B���B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�fC  C�fC�fC  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C��C��C��C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C��C��C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  D   D y�D ��D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D	  D	�fD
fD
� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D��D� DfD� D  Dy�D  D� D  D�fD  D� DfD� D��D� D  D� D  Dy�D  D� D   D �fD!  D!y�D"  D"� D#  D#�fD$  D$y�D%  D%� D%��D&�fD'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D,��D-� D.  D.y�D.��D/� D0  D0y�D1  D1� D2  D2�fD3  D3� D4  D4�fD5fD5� D6  D6�fD7  D7� D8  D8� D8��D9y�D:  D:�fD;  D;y�D<  D<� D=  D=� D>fD>� D?  D?� D@  D@� DA  DA�fDB  DBy�DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH�fDI  DI� DI��DJy�DK  DK�fDLfDL�fDM  DMy�DM��DN� DOfDO�fDPfDP� DP��DQ� DRfDR�fDS  DS� DT  DT� DT��DUy�DU��DV� DW  DW� DXfDX� DY  DY� DZ  DZ� DZ��D[y�D\  D\�fD]  D]� D^  D^� D^��D_y�D_��D`� D`��Day�Da��Dby�Db��Dcy�Dc��Dd� De  De� De��Df� DgfDg� Dh  Dh� Di  Di�fDjfDj�fDk  Dky�Dk��Dl� DmfDm� Dn  Dn� Do  Do� Do��Dp� DqfDq�fDrfDr�fDsfDs�fDt  Dty�Dt��Du� DvfDv� Dw  Dw� Dw� Dy��D�B�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@�(�A�A#�ABzAbzA�
=A��
A��
A��
A��
A��
A��
A��
B �B�B�B�B �B(�B0�B9Q�B@�BH�BP�BX�BaQ�Bh�Bp�Bx�B�u�B�B�B�u�B�u�B�B�B�B�B�u�B�u�B�B�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B�u�B��B�u�B�u�B�u�B�u�B�u�B�u�C :�C:�C:�C:�C:�C
:�C:�C!GC:�C!GC!GC:�C!GC:�C:�C:�C :�C":�C$:�C&:�C(:�C*:�C,:�C.:�C0:�C2:�C4:�C6:�C8:�C::�C<:�C>:�C@:�CB:�CD:�CF:�CH:�CJ:�CL:�CN:�CP:�CR:�CT:�CV:�CX:�CZ:�C\:�C^:�C`:�Cb:�Cd:�Cf:�Ch:�Cj:�Cl!GCn:�Cp!GCr:�Ct:�Cv:�Cx:�Cz:�C|:�C~T{C�qC�qC�qC�*>C�qC�qC�qC�qC�qC�qC�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC�qC�*>C�qC�qC�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�*>C�qC��C��C�qC�qC�qC��C�qC�qC�qC�qC�*>C�*>C�*>C�*>C�*>C�qC�qC�qC�qC��C�qC�qC�qC�qC�qC��C�qC�qC�*>C�*>C�qC��C�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC�qC��C��C�qC�qC��C�qC�qC�qC�qC�qC��C�qC�*>C�*>C�qC�qC�qC�qC�qC�*>C�*>C�*>C�*>C�qC�qC�qC�qD �D �RDRD��D�D��D�D��DRD��D�D��D�D��D�D��D�D��D	�D	�D
D
��D�D��D�D��D�D��D�D��D�D��DD�D�D��D�D��D�D��D�D��DRD��DD��D�D�RD�D��D�D�D�D��DD��DRD��D�D��D�D�RD�D��D �D �D!�D!�RD"�D"��D#�D#�D$�D$�RD%�D%��D&RD&�D'�D'��D(�D(��D)�D)��D*�D*�RD+�D+��D,�D,��D-RD-��D.�D.�RD/RD/��D0�D0�RD1�D1��D2�D2�D3�D3��D4�D4�D5D5��D6�D6�D7�D7��D8�D8��D9RD9�RD:�D:�D;�D;�RD<�D<��D=�D=��D>D>��D?�D?��D@�D@��DA�DA�DB�DB�RDC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH�DI�DI��DJRDJ�RDK�DK�DLDL�DM�DM�RDNRDN��DODO�DPDP��DQRDQ��DRDR�DS�DS��DT�DT��DURDU�RDVRDV��DW�DW��DXDX��DY�DY��DZ�DZ��D[RD[�RD\�D\�D]�D]��D^�D^��D_RD_�RD`RD`��DaRDa�RDbRDb�RDcRDc�RDdRDd��De�De��DfRDf��DgDg��Dh�Dh��Di�Di�DjDj�Dk�Dk�RDlRDl��DmDm��Dn�Dn��Do�Do��DpRDp��DqDq�DrDr�DsDs�Dt�Dt�RDuRDu��DvDv��Dw�Dw��DwθDy��D�J=D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AсAя\Aѝ�Aџ�Aљ�Aї�Aѝ�Aѡ�Aѝ�Aѝ�Aџ�Aџ�Aѥ�Aѣ�AѮAѮAѥ�Aѧ�AѰ!AѺ^AѼjAѾwAѾwA���A���A�A�A�ĜAѼjA���AѼjAѾwAѼjAѾwAѾwA���AѾwA���A�A�ƨA�ĜA�ƨA�ƨA�ȴA���A�ȴA���A���A���A���A���A���A���A���A���A�G�A�v�A�t�A�33A��/Aɏ\AȰ!Aȡ�A�C�A��A��A�-A��Aº^A�^5A�9XA�\)A��A�G�A�ȴA��A�~�A��A�5?A��mA�C�A���A�x�A�5?A�~�A�r�A�1A�E�A��PA��A�7LA��A���A��wA�5?A��wA��`A�x�A��7A�bNA���A���A���A���A��HA�E�A��-A�?}A�x�A��HA��
A���A�p�A���A���A�1A�S�A�`BA{%Ay
=Aw%Au�#At��Ar�uAm��Aj5?Agt�Af�9AcƨAb�A`ZA]�
A[�AY�AW;dAVbAS��APbNAL��AI��AHn�AG+AD��AB�DA@r�A?p�A=�
A;�hA9ƨA8v�A7�-A7
=A5�mA4�9A3�^A1��A0�RA/S�A.�uA,��A*�A)�A(ȴA(bA&z�A$�/A"bA r�AO�A�TA�yA��A�hA�yAz�A9XA��AA�A�FA�AƨA�Ap�AC�AA~�AJA�mA|�A9XA&�A�9A�A��A��AbNAp�A
�A
�A
A�A�\A��AXA�yA$�AO�A��At�A7LA �!A bN@��@���@��#@��#@���@��T@�{@�E�@�E�@��-@�v�@��
@�hs@�S�@��`@�F@�+@�Z@��H@�=q@�^@�&�@�1@�K�@�ȴ@���@��@��@�"�@�G�@�dZ@��#@ّh@�Ĝ@�b@�|�@���@��@��@��@ָR@֏\@���@�S�@���@҇+@ҟ�@�{@�J@�n�@ѡ�@Л�@�1@�K�@�5?@�/@�Ĝ@�A�@˶F@��@�^5@Ɂ@���@� �@ǅ@�33@�n�@�{@��T@�7L@ċD@�;d@�$�@���@��-@�&�@���@��@�  @���@�@�^5@���@���@�(�@�5?@�G�@�z�@�t�@��@�O�@�/@��@��@��D@�j@�Q�@�I�@�(�@��;@��w@���@�|�@�@���@��@���@�@�X@�Q�@���@��P@�C�@��@���@��P@�o@��H@���@�E�@���@�7L@���@��@�j@��@�ƨ@��@�(�@� �@��P@��@��R@�v�@���@�hs@�&�@��@��`@��@�j@�9X@���@�K�@�"�@��@�$�@���@�p�@�`B@�/@��u@�(�@���@��@�|�@�;d@��!@���@�-@���@�X@�&�@�V@���@�j@�9X@�  @���@�\)@��R@�=q@�5?@�-@���@���@��7@�x�@�?}@�%@��/@�r�@��
@��@��@���@�;d@���@�=q@���@�hs@�O�@�/@��@���@���@��@�bN@�A�@�1'@� �@��@�1@��@��
@��F@�\)@�@��@��H@�ȴ@��!@�V@��@�J@��@��-@�G�@���@��j@��u@�I�@���@���@�;d@���@��R@�ff@�=q@��@��@�?}@��@���@�z�@� �@��@��P@�t�@�S�@��@���@�V@�=q@�V@�V@��@�@��-@�G�@���@��m@���@�t�@�C�@�33@�
=@��y@�ȴ@�~�@�{@��7@�G�@�7L@�7L@�G�@�G�@�?}@�7L@��@�%@��`@��@�1@��w@���@���@�l�@�;d@�+@�o@���@���@�z@z�]@g�$111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AсAя\Aѝ�Aџ�Aљ�Aї�Aѝ�Aѡ�Aѝ�Aѝ�Aџ�Aџ�Aѥ�Aѣ�AѮAѮAѥ�Aѧ�AѰ!AѺ^AѼjAѾwAѾwA���A���A�A�A�ĜAѼjA���AѼjAѾwAѼjAѾwAѾwA���AѾwA���A�A�ƨA�ĜA�ƨA�ƨA�ȴA���A�ȴA���A���A���A���A���A���A���A���A���A�G�A�v�A�t�A�33A��/Aɏ\AȰ!Aȡ�A�C�A��A��A�-A��Aº^A�^5A�9XA�\)A��A�G�A�ȴA��A�~�A��A�5?A��mA�C�A���A�x�A�5?A�~�A�r�A�1A�E�A��PA��A�7LA��A���A��wA�5?A��wA��`A�x�A��7A�bNA���A���A���A���A��HA�E�A��-A�?}A�x�A��HA��
A���A�p�A���A���A�1A�S�A�`BA{%Ay
=Aw%Au�#At��Ar�uAm��Aj5?Agt�Af�9AcƨAb�A`ZA]�
A[�AY�AW;dAVbAS��APbNAL��AI��AHn�AG+AD��AB�DA@r�A?p�A=�
A;�hA9ƨA8v�A7�-A7
=A5�mA4�9A3�^A1��A0�RA/S�A.�uA,��A*�A)�A(ȴA(bA&z�A$�/A"bA r�AO�A�TA�yA��A�hA�yAz�A9XA��AA�A�FA�AƨA�Ap�AC�AA~�AJA�mA|�A9XA&�A�9A�A��A��AbNAp�A
�A
�A
A�A�\A��AXA�yA$�AO�A��At�A7LA �!A bN@��@���@��#@��#@���@��T@�{@�E�@�E�@��-@�v�@��
@�hs@�S�@��`@�F@�+@�Z@��H@�=q@�^@�&�@�1@�K�@�ȴ@���@��@��@�"�@�G�@�dZ@��#@ّh@�Ĝ@�b@�|�@���@��@��@��@ָR@֏\@���@�S�@���@҇+@ҟ�@�{@�J@�n�@ѡ�@Л�@�1@�K�@�5?@�/@�Ĝ@�A�@˶F@��@�^5@Ɂ@���@� �@ǅ@�33@�n�@�{@��T@�7L@ċD@�;d@�$�@���@��-@�&�@���@��@�  @���@�@�^5@���@���@�(�@�5?@�G�@�z�@�t�@��@�O�@�/@��@��@��D@�j@�Q�@�I�@�(�@��;@��w@���@�|�@�@���@��@���@�@�X@�Q�@���@��P@�C�@��@���@��P@�o@��H@���@�E�@���@�7L@���@��@�j@��@�ƨ@��@�(�@� �@��P@��@��R@�v�@���@�hs@�&�@��@��`@��@�j@�9X@���@�K�@�"�@��@�$�@���@�p�@�`B@�/@��u@�(�@���@��@�|�@�;d@��!@���@�-@���@�X@�&�@�V@���@�j@�9X@�  @���@�\)@��R@�=q@�5?@�-@���@���@��7@�x�@�?}@�%@��/@�r�@��
@��@��@���@�;d@���@�=q@���@�hs@�O�@�/@��@���@���@��@�bN@�A�@�1'@� �@��@�1@��@��
@��F@�\)@�@��@��H@�ȴ@��!@�V@��@�J@��@��-@�G�@���@��j@��u@�I�@���@���@�;d@���@��R@�ff@�=q@��@��@�?}@��@���@�z�@� �@��@��P@�t�@�S�@��@���@�V@�=q@�V@�V@��@�@��-@�G�@���@��m@���@�t�@�C�@�33@�
=@��y@�ȴ@�~�@�{@��7@�G�@�7L@�7L@�G�@�G�@�?}@�7L@��@�%@��`@��@�1@��w@���@���@�l�@�;d@�+@�o@���@���@�z@z�]@g�$111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��BbNBP�BP�BK�B49B0!B9XB7LBD�B0!B(�B+Bq�Bz�B�'B�B��BVBoB�BN�B�B�uB��B�JB}�Bz�Bp�BiyBiyBdZBVBL�BE�B8RB$�B!�B�BhBJBB�B�#B��B��B��B��B�bBcTB33B
��B
�)B
��B
�B
~�B
gmB
��B
ÖB
��B
2-B	�/B	��B	�B	p�B	aHB	XB	L�B	9XB	�B	+B��B�B�ZB�#B��BĜB�XB�B��B��B��B�\B�B~�B{�Bw�Bz�Bw�Bv�Bv�Bx�B|�B�B�%B�7B�VB�oB�{B��B��B��B��B��B�B�FB�^B�dB�}B��B�qB�LB�9B�'B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�LB�^BÖBɺB��B��B��B��B�
B�/B�`B�B�B�B�B�`B�`B�B�ZB�B�B��B��B��B	B	B	%B	+B��B�`B�/B�/B�)B�#B�#B�B�B�B�B��B�B�NB�`B�fB�mB�mB�TB�HB�BB�NB�TB�TB�TB�TB�`B�B�B��B��B��B	B	%B	JB	oB	�B	�B	�B	!�B	#�B	(�B	.B	/B	1'B	5?B	6FB	8RB	:^B	>wB	>wB	?}B	C�B	E�B	G�B	G�B	K�B	M�B	M�B	O�B	S�B	W
B	[#B	]/B	`BB	bNB	cTB	dZB	e`B	gmB	iyB	jB	jB	iyB	k�B	gmB	dZB	bNB	`BB	`BB	cTB	ffB	gmB	iyB	l�B	q�B	s�B	t�B	u�B	w�B	w�B	y�B	z�B	~�B	�B	�B	�+B	�DB	�=B	�1B	�+B	�+B	�1B	�PB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�3B	�9B	�?B	�?B	�RB	�^B	�dB	�dB	�jB	�qB	�}B	�}B	��B	��B	B	ŢB	ŢB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�5B	�;B	�BB	�HB	�BB	�HB	�HB	�TB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
	7B

=B

=B
JB
VB
VB
\B
bB
bB
hB
oB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
%�B
/�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��BbNBP�BP�BK�B49B0!B9XB7LBD�B0!B(�B+Bq�Bz�B�'B�B��BVBoB�BN�B�B�uB��B�JB}�Bz�Bp�BiyBiyBdZBVBL�BE�B8RB$�B!�B�BhBJBB�B�#B��B��B��B��B�bBcTB33B
��B
�)B
��B
�B
~�B
gmB
��B
ÖB
��B
2-B	�/B	��B	�B	p�B	aHB	XB	L�B	9XB	�B	+B��B�B�ZB�#B��BĜB�XB�B��B��B��B�\B�B~�B{�Bw�Bz�Bw�Bv�Bv�Bx�B|�B�B�%B�7B�VB�oB�{B��B��B��B��B��B�B�FB�^B�dB�}B��B�qB�LB�9B�'B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�LB�^BÖBɺB��B��B��B��B�
B�/B�`B�B�B�B�B�`B�`B�B�ZB�B�B��B��B��B	B	B	%B	+B��B�`B�/B�/B�)B�#B�#B�B�B�B�B��B�B�NB�`B�fB�mB�mB�TB�HB�BB�NB�TB�TB�TB�TB�`B�B�B��B��B��B	B	%B	JB	oB	�B	�B	�B	!�B	#�B	(�B	.B	/B	1'B	5?B	6FB	8RB	:^B	>wB	>wB	?}B	C�B	E�B	G�B	G�B	K�B	M�B	M�B	O�B	S�B	W
B	[#B	]/B	`BB	bNB	cTB	dZB	e`B	gmB	iyB	jB	jB	iyB	k�B	gmB	dZB	bNB	`BB	`BB	cTB	ffB	gmB	iyB	l�B	q�B	s�B	t�B	u�B	w�B	w�B	y�B	z�B	~�B	�B	�B	�+B	�DB	�=B	�1B	�+B	�+B	�1B	�PB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�3B	�9B	�?B	�?B	�RB	�^B	�dB	�dB	�jB	�qB	�}B	�}B	��B	��B	B	ŢB	ŢB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�/B	�5B	�;B	�BB	�HB	�BB	�HB	�HB	�TB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
1B
1B
	7B

=B

=B
JB
VB
VB
\B
bB
bB
hB
oB
oB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
+B
%�B
/�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.23 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190536                              AO  ARCAADJP                                                                    20181005190536    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190536  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190536  QCF$                G�O�G�O�G�O�8000            