CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:25Z AOML 3.0 creation; 2016-08-07T21:36:28Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221325  20160807143628  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_007                   2C  D   APEX                            6531                            072314                          846 @�V�^ 1   @�Wq� @16�t��c����o1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ�CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D�3D�9�D�s3D��fD�3D�6fD��3D��fD��3D�VfD��fD��fD�3D�33Dڌ�D��3D���D�@ D�|�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��A�\A ��AB�\Ab�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��By
=B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��RB�Q�B��B��B�Q�B�Q�B�Q�B�Q�BԅB�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C B�C(�C\C(�C(�C
(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB(�CD(�CF(�CHB�CJB�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch(�Cj(�Cl(�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|(�C~(�C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C��C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{D 
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
=D��D
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
=Dt�=Du�Dy�=D�RD�>�D�xRD�˅D�RD�;�D��RD�ۅD��RD�[�D���D�˅D�RD�8RDڑ�D��RD���D�ED��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�E�A�E�A�G�A�S�A�Q�A�Q�A�VA�S�A�S�A�XA�\)A�`BA�`BA�dZA�dZA�ffA�hsA�jA�l�A�l�A�n�A�jA�l�A�l�A�hsA�jA�ffA�S�A�;dA�A�A�G�A�7LA�Q�A�Q�A�9XA�9XA�9XA�7LA�5?A�33A�-A�%A�x�A�Q�A���A�ffA�ZA���A�5?A�%A� �A�I�A���A��TA���A�K�AA�t�A�ZA�E�A���A��A��A�33A���A���A�bNA�JA��A��A���A���A��`A���A�^5A�l�A�x�A�G�A��A��mA�+A���A�K�A��`A�hsA�Q�A���A�VA���A��A�$�A��A��hA� �A�VA�ȴA��A�A��yA�1'A��^A���A~A|��A{VAv�\Amp�AkS�Aix�Ag�
Ag?}Ae�wAcK�A_�A\(�AZ��AW��AQAO�hAN$�AM�7AJv�AC�hAAC�A@$�A?�A>9XA<n�A:ZA8��A8A�A7��A6�!A4��A3`BA2n�A2bA0�9A/�A.ffA-%A+�hA+�A(A'oA'
=A&�/A%A#��A#
=A"�DA ZA�
A��A�RA��A�Az�A�A��A�\Ar�A-A�hAhsAK�A�9A{A��A�DA^5AA�A�A33A��A-A�7A33A�jAƨAK�AK�A�AE�A�;Al�A	�AA�A��AC�A%A�A�jA�\A=qA  A�
AƨA`BA��A�AƨA�;AƨAl�A�hA�A�-A �!@�\)@�%@�^5@�b@�V@�@�M�@��@�ȴ@�v�@�~�@�n�@���@�z�@��H@�G�@�v�@ߥ�@߶F@��;@�b@�9@�hs@�^@�5?@�R@�+@�;d@�;d@�"�@�{@��u@���@��@�1'@�5?@�t�@�v�@���@�ff@���@�O�@�n�@�7L@�M�@���@�Q�@ύP@�=q@̼j@�l�@ʸR@���@�ȴ@ʧ�@ɲ-@���@�Ĝ@Ȭ@�ƨ@ǥ�@ƸR@�{@�`B@�hs@� �@���@Ĵ9@���@ě�@Ý�@��@þw@�@�@�x�@���@���@��`@��;@��
@�ƨ@��F@���@�dZ@��R@�^5@���@��R@���@�=q@���@�?}@��@��/@��/@��@��D@�Q�@�b@�\)@���@�5?@��@��h@�V@�V@���@���@�A�@�b@�ƨ@��@���@��@�|�@�S�@�+@�ȴ@�E�@�$�@���@�hs@�O�@��@���@���@�I�@�b@��m@���@�\)@���@�5?@��-@��@�X@��@���@��u@��D@��u@���@�j@�(�@��m@��w@���@�l�@�;d@�@��H@���@��@�O�@�%@�j@�b@�ƨ@�\)@��y@���@�-@�{@��@��@�x�@�hs@�7L@��/@��D@�9X@��@�  @�ƨ@���@�;d@��@�;d@��P@��w@���@��P@�33@��y@�ȴ@��+@�V@�J@��^@�?}@���@��@���@��D@�r�@� �@���@��R@�^5@�$�@���@���@��7@�7L@��@���@��@���@��@��@�A�@��@���@��
@�ƨ@�|�@��@�~�@�5?@��@��-@��@��`@���@��j@���@�bN@�  @���@��P@�
=@�v�@�5?@��@��^@�`B@�&�@���@��D@�I�@�1@���@�ȴ@�n�@�5?@��@��^@�x�@�&�@��/@��j@�z�@�(�@���@���@�t�@�@�n�@�@�hs@�hs@�X@�G�@�7L@�%@���@��D@�  @�ƨ@��@�|�@�C�@���@���@�v�@�=q@��@���@���@�dZ@��@��j@�P@v@hr�@_�@Vff@M�@D�@>V@8A�@1x�@+�
@%O�@ A�@o@�@�@�
@r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�E�A�E�A�G�A�S�A�Q�A�Q�A�VA�S�A�S�A�XA�\)A�`BA�`BA�dZA�dZA�ffA�hsA�jA�l�A�l�A�n�A�jA�l�A�l�A�hsA�jA�ffA�S�A�;dA�A�A�G�A�7LA�Q�A�Q�A�9XA�9XA�9XA�7LA�5?A�33A�-A�%A�x�A�Q�A���A�ffA�ZA���A�5?A�%A� �A�I�A���A��TA���A�K�AA�t�A�ZA�E�A���A��A��A�33A���A���A�bNA�JA��A��A���A���A��`A���A�^5A�l�A�x�A�G�A��A��mA�+A���A�K�A��`A�hsA�Q�A���A�VA���A��A�$�A��A��hA� �A�VA�ȴA��A�A��yA�1'A��^A���A~A|��A{VAv�\Amp�AkS�Aix�Ag�
Ag?}Ae�wAcK�A_�A\(�AZ��AW��AQAO�hAN$�AM�7AJv�AC�hAAC�A@$�A?�A>9XA<n�A:ZA8��A8A�A7��A6�!A4��A3`BA2n�A2bA0�9A/�A.ffA-%A+�hA+�A(A'oA'
=A&�/A%A#��A#
=A"�DA ZA�
A��A�RA��A�Az�A�A��A�\Ar�A-A�hAhsAK�A�9A{A��A�DA^5AA�A�A33A��A-A�7A33A�jAƨAK�AK�A�AE�A�;Al�A	�AA�A��AC�A%A�A�jA�\A=qA  A�
AƨA`BA��A�AƨA�;AƨAl�A�hA�A�-A �!@�\)@�%@�^5@�b@�V@�@�M�@��@�ȴ@�v�@�~�@�n�@���@�z�@��H@�G�@�v�@ߥ�@߶F@��;@�b@�9@�hs@�^@�5?@�R@�+@�;d@�;d@�"�@�{@��u@���@��@�1'@�5?@�t�@�v�@���@�ff@���@�O�@�n�@�7L@�M�@���@�Q�@ύP@�=q@̼j@�l�@ʸR@���@�ȴ@ʧ�@ɲ-@���@�Ĝ@Ȭ@�ƨ@ǥ�@ƸR@�{@�`B@�hs@� �@���@Ĵ9@���@ě�@Ý�@��@þw@�@�@�x�@���@���@��`@��;@��
@�ƨ@��F@���@�dZ@��R@�^5@���@��R@���@�=q@���@�?}@��@��/@��/@��@��D@�Q�@�b@�\)@���@�5?@��@��h@�V@�V@���@���@�A�@�b@�ƨ@��@���@��@�|�@�S�@�+@�ȴ@�E�@�$�@���@�hs@�O�@��@���@���@�I�@�b@��m@���@�\)@���@�5?@��-@��@�X@��@���@��u@��D@��u@���@�j@�(�@��m@��w@���@�l�@�;d@�@��H@���@��@�O�@�%@�j@�b@�ƨ@�\)@��y@���@�-@�{@��@��@�x�@�hs@�7L@��/@��D@�9X@��@�  @�ƨ@���@�;d@��@�;d@��P@��w@���@��P@�33@��y@�ȴ@��+@�V@�J@��^@�?}@���@��@���@��D@�r�@� �@���@��R@�^5@�$�@���@���@��7@�7L@��@���@��@���@��@��@�A�@��@���@��
@�ƨ@�|�@��@�~�@�5?@��@��-@��@��`@���@��j@���@�bN@�  @���@��P@�
=@�v�@�5?@��@��^@�`B@�&�@���@��D@�I�@�1@���@�ȴ@�n�@�5?@��@��^@�x�@�&�@��/@��j@�z�@�(�@���@���@�t�@�@�n�@�@�hs@�hs@�X@�G�@�7L@�%@���@��D@�  @�ƨ@��@�|�@�C�@���@���@�v�@�=q@��@���G�O�@�dZ@��@��j@�P@v@hr�@_�@Vff@M�@D�@>V@8A�@1x�@+�
@%O�@ A�@o@�@�@�
@r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBbBbBbBbBbBbBbBbBbBbBbBbBbBbBbBbBbBbBbBbBbBbBbBbBbBbBbB\BVBVBVBVBVB\BVBVBVBVBPBPBJBDB1B
=B%B  B
�B
��B�B,BG�BVBZBm�B� B�7B��B�LB�^B�jB��B�TB1BJBVB�B�B�BhB+B�B�B�B�B��B�B�B�NB�mB��B��B��B�-B�uB�uB�DB�Bs�B�oB�B��B}�B>wB
��B
��B
��B
�B
aHB
C�B
=qB
/B
VB	�B	�B	�sB	��B	y�B	e`B	`BB	gmB	n�B	bNB	YB	B�B	/B	%�B	�B�B�fB�;B�#B�
B��B�dB�XB�RB�?B�-B�-B�'B�'B�'B�!B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB��B��B��B�9B�}B��BƨBȴB��B��B�5B�ZB�ZB�TB�NB�sB�B��B��B	  B	B	B	  B��B	B	B	  B	+B	oB	bB	VB	JB	1B	  B	B	B	1B		7B	
=B	DB	JB	DB	PB	hB	�B	(�B	$�B	�B	!�B	(�B	5?B	8RB	;dB	=qB	<jB	6FB	-B	$�B	 �B	�B	�B	�B	�B	�B	{B	�B	�B	�B	�B	�B	�B	�B	bB	JB	uB	�B	 �B	'�B	/B	2-B	7LB	@�B	H�B	J�B	K�B	L�B	N�B	J�B	G�B	B�B	>wB	>wB	S�B	VB	Q�B	G�B	F�B	G�B	S�B	Q�B	H�B	E�B	F�B	D�B	@�B	@�B	H�B	L�B	N�B	P�B	T�B	S�B	R�B	S�B	YB	\)B	`BB	_;B	^5B	_;B	bNB	aHB	cTB	m�B	p�B	p�B	m�B	q�B	s�B	t�B	t�B	u�B	w�B	{�B	z�B	}�B	~�B	~�B	� B	�B	�B	�+B	�JB	�bB	�uB	�{B	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�9B	�?B	�?B	�FB	�LB	�LB	�RB	�XB	�^B	�jB	�jB	�jB	�jB	�qB	�wB	��B	��B	��B	B	ÖB	ĜB	ĜB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�)B	�/B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
%B
VB
�B
�B
�B
%�B
/B
5?B
9XB
B�B
H�B
M�B
S�B
YB
_;B
cTB
hsB
m�B
q�B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BWBZBUBVBVBXBVBYBVBVBZBVBVBVBYBYBYBUBVBXBXBXBVBTBXBXBXBSBKBKBLBMBKBQBKBKBKBLBFBFBAB9B'B
2BB
��B
�~B
��B{B+�BG�BU�BZBm�B�B�+B��B�>B�QB�]B�{B�DB#B;BDBwB�ByBXBB�B�xB�B�B��B�B�B�?B�_B��B��B��B�B�fB�gB�6B��Bs�B�_B�
B��B}�B>jB
��B
˹B
��B
�B
a=B
C�B
=fB
/B
NB	�B	�B	�oB	��B	y�B	e\B	`>B	gkB	n�B	bJB	YB	B�B	/B	%�B	B�B�gB�<B�$B�B��B�fB�YB�RB�CB�0B�1B�+B�)B�)B�$B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�xB��B��B��B�:B�|B��BƤBȴB��B��B�5B�XB�XB�SB�MB�sB�B��B��B��B	B	B��B��B		B	B��B	'B	jB	^B	RB	FB	.B��B	B	B	.B		4B	
8B	@B	FB	@B	LB	eB	�B	(�B	$�B	�B	!�B	(�B	58B	8KB	;_B	=kB	<cB	6?B	-	B	$�B	 �B	�B	�B	�B	�B	�B	tB	�B	�B	�B	~B	zB	�B	�B	]B	DB	pB	�B	 �B	'�B	/B	2(B	7DB	@|B	H�B	J�B	K�B	L�B	N�B	J�B	G�B	B�B	>qB	>pB	S�B	U�B	Q�B	G�B	F�B	G�B	S�B	Q�B	H�B	E�B	F�B	D�B	@yB	@}B	H�B	L�B	N�B	P�B	T�B	S�B	R�B	S�B	YB	\B	`9B	_1B	^+B	_3B	bEB	a@B	cLB	m�B	p�B	p�B	m�B	q�B	s�B	t�B	t�B	u�B	w�B	{�B	z�B	}�B	~�B	~�B	�B	��B	�B	� B	�@B	�WB	�iB	�sB	�rB	�sB	�pB	�qB	�{B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	� B	�+B	�1B	�2B	�8B	�@B	�BB	�EB	�LB	�SB	�_B	�]B	�^B	�_B	�dB	�mB	�vB	�}B	�{B	B	ËB	ĐB	ďB	ǢB	ǢB	ǤB	ɯB	ʴB	ʷB	ʳB	˺B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�	B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	�/B	�3B	�4B	�2B	�4B	�;B	�8B	�9B	�<B	�9B	�<B	�AB	�FB	�MB	�SB	�RB	�ZB	�[B	�WB	�aB	�fB	�lB	�oB	�qB	�pB	�wB	�{B	�vB	�wB	�}B	�B	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
B
G�O�B
B
JB
xB
�B
�B
%�B
/B
5-B
9JB
B�B
H�B
M�B
S�B
YB
_+B
cEB
hbB
m~B
q�B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.16 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436282016080714362820160807143628  AO  ARCAADJP                                                                    20150226221325    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221325  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221325  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143628  IP                  G�O�G�O�G�O�                