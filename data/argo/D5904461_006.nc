CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:24Z AOML 3.0 creation; 2016-08-07T21:36:28Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221324  20160807143628  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_006                   2C  D   APEX                            6531                            072314                          846 @��E��1   @�:� @12���m�c�x���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D��D�9�D���D��3D��fD�C3D�� D�ٚD�	�D�,�D���D���D��D�I�Dڃ3D���D��fD�@ D�i�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��A�\A"�\AB�\Ab�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bq
=Bx��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B�Q�B��B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B�Q�B�Q�B�Q�B�Q�B�Q�C (�C(�C(�C(�C(�C
B�C(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4B�C6(�C8(�C:\C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch(�Cj(�ClB�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|(�C~(�C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{D 
=D �=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D	
=D	�=D

=D
�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D 
=D �=D!
=D!�=D"
=D"�=D#
=D#�=D$
=D$�=D%
=D%�=D&
=D&�=D'
=D'�=D(
=D(�=D)
=D)�=D*
=D*�=D+
=D+�=D,
=D,�=D-
=D-�=D.
=D.�=D/
=D/�=D0
=D0�=D1
=D1�=D2
=D2�=D3
=D3�=D4
=D4�=D5
=D5�=D6
=D6�=D7
=D7�=D8
=D8�=D9
=D9�=D:
=D:�=D;
=D;�=D<
=D<�=D=
=D=�=D>
=D>�=D?
=D?�=D@
=D@�=DA
=DA�=DB
=DB�=DC
=DC�=DD
=DD�=DE
=DE�=DF
=DF�=DG
=DG�=DH
=DH�=DI
=DI�=DJ
=DJ�=DK
=DK�=DL
=DL�=DM
=DM�=DN
=DN�=DO
=DO�=DP
=DP�=DQ
=DQ�=DR
=DR�=DS
=DS�=DT
=DT�=DU
=DU�=DV
=DV�=DW
=DW�=DX
=DX�=DY
=DY�=DZ
=DZ�=D[
=D[�=D\
=D\�=D]
=D]�=D^
=D^�=D_
=D_�=D`
=D`�=Da
=Da�=Db
=Db�=Dc
=Dc�=Dd
=Dd�=De
=De�=Df
=Df�=Dg
=Dg�=Dh
=Dh�=Di
=Di�=Dj
=Dj�=Dk
=Dk�=Dl
=Dl�=Dm
=Dm�=Dn
=Dn�=Do
=Do�=Dp
=Dp�=Dq
=Dq�=Dr
=Dr�=Ds
=Ds�=Dt
=Dt��Dy��D�!�D�>�D���D��RD���D�HRD��D�޹D��D�1�D���D���D��D�N�DڈRD���D���D�ED�n�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�r�A�p�A�r�A�t�A�v�A�x�A�|�A�|�A�z�A�z�A�|�A�~�A܁A܁A�~�A܁A�~�A�~�A܁A܅A܅A܃A܃A܅A܉7A܍PA܍PA܍PAܓuAܓuAܓuAܗ�AܓuA܍PA܍PA܉7A܉7A܋DA܋DA܇+A�p�A�XA٬A��mA�
=A��A��#A�z�A�v�A�p�AȋDA�K�AŃA�O�A�bAËDA�ȴA�oA��7A�ZA�{A��\A�A�v�A�33A�A�A�A�ffA�/A���A�O�A�M�A���A���A��A�JA��HA���A���A�z�A���A�r�A�VA��\A��A��+A�t�A��wA�r�A�S�A���A���A�A���A��`A�z�A�7LA�9XA�K�A�\)A��A��/A��hA�33A�33A�+A�^5A�`BA~��A|ĜAw�mAr�Ao
=Ai�Ai�AgAb �A^�+A[XAU?}AS�AR��AQ�AK�mAJI�AIdZAH�uAG��AD�/AC�ABv�AA|�AAC�A@��A@^5A?%A<�/A;|�A9�A5�wA2��A/�TA-oA*5?A)%A'��A'�A$�A"�A��AĜA �A�A�A�A~�A��A$�A��A��A%An�AQ�A�A�A�
A33A��A��A
=A�A��A �A�A�DA�jAbAbNA~�AM�AA�#A��AC�A%A��AoA(�A�A
{A	��A	�A1A�A5?A�^At�AK�A+A��AffA�#A�A�DA�uA\)A 1@���@�33@���@��m@�o@��@��u@�K�@��^@�A�@�!@���@�hs@�u@�dZ@��@�Q�@�1'@�@�O�@�7L@���@��
@��T@݁@�hs@�
=@��@���@��y@���@�`B@�  @�o@�7L@�%@���@ܣ�@�I�@ܬ@ܬ@܋D@ܛ�@ܬ@�Q�@ڰ!@��@�ȴ@���@��m@���@�~�@��/@�t�@��H@�M�@̃@˅@��y@ɉ7@���@�O�@�?}@��@�Z@ǶF@Ɵ�@�J@š�@ř�@őh@�p�@�?}@�O�@ě�@þw@��@���@\@°!@�ff@�ȴ@�^5@���@��@�j@�1'@���@�(�@���@��\@�M�@�$�@�@��7@���@��@���@�z�@�I�@��w@�C�@�5?@�x�@�&�@��@��9@� �@�ƨ@�l�@��@��@���@�~�@�5?@��@���@��u@��@�r�@�Q�@���@�33@��H@��!@���@�E�@��h@�p�@�`B@�/@�V@���@�r�@�  @���@�\)@�l�@��@�+@�ȴ@�5?@���@�@�p�@�?}@�&�@��`@��9@�I�@�  @���@�"�@�E�@��@��#@�X@���@���@���@��u@�r�@�Q�@�  @�ƨ@�|�@�;d@��y@��!@���@��+@�v�@�n�@�ff@�^5@��#@��^@�x�@�V@���@��j@��D@�  @���@�K�@���@��+@�M�@���@�@�=q@���@�X@�O�@��@���@��@�V@�%@��`@��j@��u@�r�@�(�@��;@���@�S�@�33@�o@��y@��R@��!@�-@�@��@�/@��9@���@���@��D@�j@� �@�ƨ@�\)@��@�
=@�~�@��@���@��T@���@�@���@�`B@�V@��@��j@��@�Q�@�1'@��m@�dZ@���@��!@��+@�@���@�7L@�V@��@��D@�I�@�1@��@��
@���@�ƨ@�dZ@�"�@���@�ȴ@���@���@�5?@��@���@��@�O�@�O�@�?}@�V@��j@��u@�1@��w@��@�K�@�;d@���@���@�ff@��@��R@��h@w�;@m�h@e@]V@O|�@Gl�@@ �@;�
@7�@0��@*�@"=q@��@��@E�@&�@1@r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�r�A�p�A�r�A�t�A�v�A�x�A�|�A�|�A�z�A�z�A�|�A�~�A܁A܁A�~�A܁A�~�A�~�A܁A܅A܅A܃A܃A܅A܉7A܍PA܍PA܍PAܓuAܓuAܓuAܗ�AܓuA܍PA܍PA܉7A܉7A܋DA܋DA܇+A�p�A�XA٬A��mA�
=A��A��#A�z�A�v�A�p�AȋDA�K�AŃA�O�A�bAËDA�ȴA�oA��7A�ZA�{A��\A�A�v�A�33A�A�A�A�ffA�/A���A�O�A�M�A���A���A��A�JA��HA���A���A�z�A���A�r�A�VA��\A��A��+A�t�A��wA�r�A�S�A���A���A�A���A��`A�z�A�7LA�9XA�K�A�\)A��A��/A��hA�33A�33A�+A�^5A�`BA~��A|ĜAw�mAr�Ao
=Ai�Ai�AgAb �A^�+A[XAU?}AS�AR��AQ�AK�mAJI�AIdZAH�uAG��AD�/AC�ABv�AA|�AAC�A@��A@^5A?%A<�/A;|�A9�A5�wA2��A/�TA-oA*5?A)%A'��A'�A$�A"�A��AĜA �A�A�A�A~�A��A$�A��A��A%An�AQ�A�A�A�
A33A��A��A
=A�A��A �A�A�DA�jAbAbNA~�AM�AA�#A��AC�A%A��AoA(�A�A
{A	��A	�A1A�A5?A�^At�AK�A+A��AffA�#A�A�DA�uA\)A 1@���@�33@���@��m@�o@��@��u@�K�@��^@�A�@�!@���@�hs@�u@�dZ@��@�Q�@�1'@�@�O�@�7L@���@��
@��T@݁@�hs@�
=@��@���@��y@���@�`B@�  @�o@�7L@�%@���@ܣ�@�I�@ܬ@ܬ@܋D@ܛ�@ܬ@�Q�@ڰ!@��@�ȴ@���@��m@���@�~�@��/@�t�@��H@�M�@̃@˅@��y@ɉ7@���@�O�@�?}@��@�Z@ǶF@Ɵ�@�J@š�@ř�@őh@�p�@�?}@�O�@ě�@þw@��@���@\@°!@�ff@�ȴ@�^5@���@��@�j@�1'@���@�(�@���@��\@�M�@�$�@�@��7@���@��@���@�z�@�I�@��w@�C�@�5?@�x�@�&�@��@��9@� �@�ƨ@�l�@��@��@���@�~�@�5?@��@���@��u@��@�r�@�Q�@���@�33@��H@��!@���@�E�@��h@�p�@�`B@�/@�V@���@�r�@�  @���@�\)@�l�@��@�+@�ȴ@�5?@���@�@�p�@�?}@�&�@��`@��9@�I�@�  @���@�"�@�E�@��@��#@�X@���@���@���@��u@�r�@�Q�@�  @�ƨ@�|�@�;d@��y@��!@���@��+@�v�@�n�@�ff@�^5@��#@��^@�x�@�V@���@��j@��D@�  @���@�K�@���@��+@�M�@���@�@�=q@���@�X@�O�@��@���@��@�V@�%@��`@��j@��u@�r�@�(�@��;@���@�S�@�33@�o@��y@��R@��!@�-@�@��@�/@��9@���@���@��D@�j@� �@�ƨ@�\)@��@�
=@�~�@��@���@��T@���@�@���@�`B@�V@��@��j@��@�Q�@�1'@��m@�dZ@���@��!@��+@�@���@�7L@�V@��@��D@�I�@�1@��@��
@���@�ƨ@�dZ@�"�@���@�ȴ@���@���@�5?@��@���@��@�O�@�O�@�?}@�V@��j@��u@�1@��w@��@�K�@�;d@���@���G�O�@��@��R@��h@w�;@m�h@e@]V@O|�@Gl�@@ �@;�
@7�@0��@*�@"=q@��@��@E�@&�@1@r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBPBJB	7B
��B
�BBB
�B
�mB
��B1'BJ�BXBffB}�B�B�7B�B��B�B�B�B��B��BB1BJBbB�B!�B�B�B%B�yB�ZB�;B�BB�ZB�`B�/B��BĜBƨBǮBB�9B�B��B��B�JB�By�Bk�BS�BB�B-B0!B�B
�B
ƨB
�dB
�!B
�+B
iyB
B�B
9XB
<jB
:^B
49B
!�B
PB	�B	��B	��B	�B	��B	�bB	r�B	\)B	B�B	�B	{B	uB	JB��B�B�yB�ZB�;B��B��BB�wB�qB�dB�LB�9B�'B�B��B��B��B��B��B��B��B��B��B�{B�hB�bB�hB�oB�oB�bB��B��B�B�-B�wBBǮB��B��B��B��B��B�)B�BB�)B�NB	B	
=B	\B	bB	\B	�B	�B	!�B	%�B	'�B	)�B	49B	8RB	;dB	:^B	<jB	A�B	A�B	?}B	<jB	@�B	>wB	33B	&�B	&�B	&�B	5?B	8RB	9XB	9XB	7LB	49B	33B	7LB	=qB	49B	2-B	1'B	0!B	-B	,B	.B	'�B	)�B	.B	+B	+B	(�B	&�B	&�B	%�B	$�B	'�B	%�B	�B	�B	�B	!�B	(�B	$�B	�B	VB	uB	"�B	/B	C�B	Q�B	Q�B	O�B	L�B	K�B	I�B	H�B	H�B	H�B	R�B	\)B	`BB	aHB	bNB	dZB	cTB	^5B	VB	P�B	M�B	J�B	G�B	E�B	@�B	=qB	@�B	E�B	G�B	I�B	L�B	K�B	N�B	T�B	VB	XB	[#B	bNB	cTB	cTB	dZB	dZB	e`B	e`B	e`B	gmB	ffB	ffB	ffB	hsB	k�B	q�B	w�B	~�B	�B	� B	� B	~�B	�B	�1B	�7B	�JB	�PB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�LB	�XB	�dB	�wB	�}B	�qB	�jB	�}B	��B	��B	��B	��B	ÖB	ŢB	ƨB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�B	�B	�B	�B	�B	�B	�#B	�;B	�;B	�BB	�BB	�HB	�HB	�HB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�fB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
1B
uB
�B
�B
 �B
%�B
/B
8RB
@�B
F�B
K�B
N�B
T�B
[#B
aHB
e`B
iyB
m�B
q�B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   BJBKBOBLBJBKBNBKBLBNBKBLBKBKBKBKBLBLBKBLBLBNBNBLBNBLBLBNBKBLBLBNBNBNBLBNBLBNBNBDB>B	.B
��B
�B �BB
�B
�eB
��B1BJ�BXBfYB}�B��B�,B�B��B�xB�{B�B��B��B �B#B8BSB�B!�B�B{BB�kB�KB�-B�5B�JB�RB� B˺BĎBƚBǡBB�(B�B��B��B�;B�By�BkuBS�BB�B,�B0B�B
�B
ƛB
�WB
�B
� B
ilB
B�B
9MB
<^B
:VB
4/B
!�B
JB	�xB	��B	��B	�B	��B	�^B	r�B	\%B	B�B	�B	zB	uB	IB��B�B�{B�^B�;B��B��BB�{B�rB�iB�NB�=B�)B�B�B��B��B��B��B��B��B��B��B�~B�lB�fB�iB�qB�qB�dB��B��B�B�.B�vBBǬB��B��B��B��B��B�'B�@B�%B�OB	B	
:B	WB	_B	XB	�B	�B	!�B	%�B	'�B	)�B	44B	8KB	;_B	:YB	<cB	A�B	A�B	?xB	<dB	@~B	>pB	3,B	&�B	&�B	&�B	5;B	8MB	9SB	9TB	7EB	44B	3-B	7EB	=lB	43B	2&B	1 B	0B	-B	,B	.B	'�B	)�B	.B	*�B	*�B	(�B	&�B	&�B	%�B	$�B	'�B	%�B	�B	�B	�B	!�B	(�B	$�B	�B	PB	qB	"�B	/B	C�B	Q�B	Q�B	O�B	L�B	K�B	I�B	H�B	H�B	H�B	R�B	\B	`8B	a>B	bDB	dRB	cJB	^+B	U�B	P�B	M�B	J�B	G�B	E�B	@}B	=kB	@{B	E�B	G�B	I�B	L�B	K�B	N�B	T�B	U�B	XB	[B	bDB	cMB	cOB	dSB	dQB	eVB	eVB	eUB	geB	f[B	fZB	f\B	hlB	kB	q�B	w�B	~�B	��B	�B	�B	~�B	�	B	�)B	�.B	�?B	�FB	�QB	�cB	�iB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�*B	�1B	�4B	�2B	�3B	�1B	�0B	�1B	�2B	�AB	�LB	�XB	�mB	�oB	�dB	�^B	�pB	�}B	�~B	�}B	�|B	ÈB	ŖB	ƝB	ȧB	ȦB	ȦB	ʳB	˺B	˺B	̿B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�
B	�
B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�	B	�B	�B	�B	�
B	�B	�.B	�-B	�4B	�4B	�:B	�9B	�<B	�@B	�FB	�EB	�IB	�FB	�HB	�FB	�HB	�DB	�LB	�LB	�LB	�UB	�YB	�YB	�dB	�fB	�gB	�mB	�kB	�pB	�xB	�xB	�wB	�xB	�xB	�yB	�vB	�xB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
G�O�B
"B
hB
�B
�B
 �B
%�B
/B
8AB
@qB
F�B
K�B
N�B
T�B
[B
a4B
eMB
ijB
m�B
q�B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.16 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436282016080714362820160807143628  AO  ARCAADJP                                                                    20150226221324    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221324  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221324  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143628  IP                  G�O�G�O�G�O�                