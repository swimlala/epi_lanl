CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:16:31Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190219181631  20200831164617  5903273 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  3334                            2C  D   APEX                            4917                            041310                          846 @Ց��3b1   @Ցނ�<�@6���R�c�ě��T1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A���A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D�  D�:=D���D�� D�
�D�<)D���D��)D�fD�J�D��qD�ǮD��\D�7�D�p�D�HD��qD�B=D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @C�@��@��A�\A"�\AB�\Ab�\A�G�A�G�A�G�A�G�A�{A�G�A�{A�G�B ��B��B��B��B!
=B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C (�C(�C\C(�C(�C
(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C(B�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch(�Cj(�Cl(�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|(�C~(�C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{D 
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
=Dp��Dq
=Dq�=Dr
=Dr�=Ds
=Ds�=Dt
=Dt�=Dy�3D�D�?\D��D��D� D�AHD���D��HD��D�O�D���D���D��{D�<�D�vD�gD��D�G\D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA��mA��yA���A���A���A��A���A���A���A���A��A��/A�z�A�oA�G�Aß�A�AA���A�Q�A��A��DA�  A�/A�%A��A��A��/A���A�I�A���A�`BA�?}A�+A�JA���A���A�\)A���A���A�r�A�bNA�Q�A���A�7LA�oA���A��RA�dZA�"�A��A�~�A�%A�(�A��A��A�VA��;A�A���A�bNA�JA��jA��uA�l�A��A�5?A�r�A�"�A���A���A�ȴA�+A��HA��A��A���A��yA��A�hsA�jA�ƨA�~�A�;dA� �A��-A��A�O�A��9A�G�A��wA�v�A��wA���A��`A��FA��!A���A�-A�p�A��-A�l�A���A�oA�/A���A���A��/A�XA��hA�"�A�M�A�VA��mA�C�A��A��TA�=qA��-A��\A�^5A���A�I�A���A�z�A�
=A\)A}��A|�!Az�HAy�PAyVAxVAu�FAr��Aq33Al��Ak�7Aj�`Ai�;Ai�Ael�Aa�TA^jA[K�AV�`AS��AQ��AP�jAP~�APE�AOhsAN9XAM?}AJ��AI�7AHQ�AG�AE�AD��AB�A@��A>�DA<ffA;x�A:z�A:bA8�/A7�A4=qA1ƨA/ƨA.E�A-��A,��A,v�A+�A+�A+O�A*�!A*ZA)��A'��A'C�A$�A#��A#%A n�A��A��A��A\)A�An�A�yA-A �A�;A&�AĜA�A�Az�A�RA�`AK�A&�AbNA��A%A��AI�A\)Az�A�A�#Al�A�A��A/A
bNA	%A�A9XA��AK�A�HA��A�DAQ�A7LA�^A{A$�A\)AƨA33A+A�A �@��`@��+@�hs@�&�@�z�@�E�@�/@���@��@��@�K�@� �@�P@�@��@�l�@�^@߮@�x�@٩�@��@�@У�@��
@���@�-@��@ͩ�@�p�@��@��@̴9@�1'@��@�x�@� �@�o@��@��@��/@�I�@§�@� �@�"�@���@�/@��@�Q�@���@�-@�X@���@��u@��
@�|�@�t�@�dZ@��@���@�5?@���@���@���@�Ĝ@�Z@�b@��;@�o@���@���@���@��h@���@��@�|�@�"�@���@�r�@��@�1@��
@�ƨ@�|�@�K�@�@��!@�~�@�=q@��h@���@�1'@���@�+@���@�V@��@��@��@�I�@��
@��@��P@�l�@�C�@��@���@�M�@�-@�$�@��@��T@���@���@��@��`@��j@�A�@��F@�33@��H@���@���@�~�@�V@�$�@���@�7L@�V@��@���@�z�@� �@�1@��
@�|�@���@�t�@��@���@�v�@�J@���@��`@�b@���@�|�@�+@�ȴ@��+@�E�@�@�%@�I�@���@�S�@�;d@�"�@���@��!@�ff@�5?@��^@�`B@��@�%@���@���@�j@�(�@�(�@�1'@�1'@� �@��@�9X@��@���@��F@���@�|�@�t�@��@�|�@�o@�M�@��@�{@��^@�X@�?}@�/@���@��9@��D@��D@�z�@��@�1'@�(�@�  @��;@�dZ@��H@���@��H@���@�(�@�Q�@�Q�@�(�@�b@���@��F@���@��P@��@�dZ@�+@��@��@���@��@��-@���@�`B@�?}@�&�@���@��`@�Ĝ@���@��u@�(�@��m@��;@�ƨ@��F@���@��@�l�@�33@���@���@�V@�$�@�{@�J@��^@�a|@|��@r�L@g=@^0U@WK�@Q�@K�@Hb@A%F@9�@57L@.Ta@('R@$oi@ 4n@�<@d�@S�@o�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��TA��mA��yA���A���A���A��A���A���A���A���A��A��/A�z�A�oA�G�Aß�A�AA���A�Q�A��A��DA�  A�/A�%A��A��A��/A���A�I�A���A�`BA�?}A�+A�JA���A���A�\)A���A���A�r�A�bNA�Q�A���A�7LA�oA���A��RA�dZA�"�A��A�~�A�%A�(�A��A��A�VA��;A�A���A�bNA�JA��jA��uA�l�A��A�5?A�r�A�"�A���A���A�ȴA�+A��HA��A��A���A��yA��A�hsA�jA�ƨA�~�A�;dA� �A��-A��A�O�A��9A�G�A��wA�v�A��wA���A��`A��FA��!A���A�-A�p�A��-A�l�A���A�oA�/A���A���A��/A�XA��hA�"�A�M�A�VA��mA�C�A��A��TA�=qA��-A��\A�^5A���A�I�A���A�z�A�
=A\)A}��A|�!Az�HAy�PAyVAxVAu�FAr��Aq33Al��Ak�7Aj�`Ai�;Ai�Ael�Aa�TA^jA[K�AV�`AS��AQ��AP�jAP~�APE�AOhsAN9XAM?}AJ��AI�7AHQ�AG�AE�AD��AB�A@��A>�DA<ffA;x�A:z�A:bA8�/A7�A4=qA1ƨA/ƨA.E�A-��A,��A,v�A+�A+�A+O�A*�!A*ZA)��A'��A'C�A$�A#��A#%A n�A��A��A��A\)A�An�A�yA-A �A�;A&�AĜA�A�Az�A�RA�`AK�A&�AbNA��A%A��AI�A\)Az�A�A�#Al�A�A��A/A
bNA	%A�A9XA��AK�A�HA��A�DAQ�A7LA�^A{A$�A\)AƨA33A+A�A �@��`@��+@�hs@�&�@�z�@�E�@�/@���@��@��@�K�@� �@�P@�@��@�l�@�^@߮@�x�@٩�@��@�@У�@��
@���@�-@��@ͩ�@�p�@��@��@̴9@�1'@��@�x�@� �@�o@��@��@��/@�I�@§�@� �@�"�@���@�/@��@�Q�@���@�-@�X@���@��u@��
@�|�@�t�@�dZ@��@���@�5?@���@���@���@�Ĝ@�Z@�b@��;@�o@���@���@���@��h@���@��@�|�@�"�@���@�r�@��@�1@��
@�ƨ@�|�@�K�@�@��!@�~�@�=q@��h@���@�1'@���@�+@���@�V@��@��@��@�I�@��
@��@��P@�l�@�C�@��@���@�M�@�-@�$�@��@��T@���@���@��@��`@��j@�A�@��F@�33@��H@���@���@�~�@�V@�$�@���@�7L@�V@��@���@�z�@� �@�1@��
@�|�@���@�t�@��@���@�v�@�J@���@��`@�b@���@�|�@�+@�ȴ@��+@�E�@�@�%@�I�@���@�S�@�;d@�"�@���@��!@�ff@�5?@��^@�`B@��@�%@���@���@�j@�(�@�(�@�1'@�1'@� �@��@�9X@��@���@��F@���@�|�@�t�@��@�|�@�o@�M�@��@�{@��^@�X@�?}@�/@���@��9@��D@��D@�z�@��@�1'@�(�@�  @��;@�dZ@��H@���@��H@���@�(�@�Q�@�Q�@�(�@�b@���@��F@���@��P@��@�dZ@�+@��@��@���@��@��-@���@�`B@�?}@�&�@���@��`@�Ĝ@���@��u@�(�@��m@��;@�ƨ@��F@���@��@�l�@�33@���@���@�V@�$�@�{@�JG�O�@�a|@|��@r�L@g=@^0U@WK�@Q�@K�@Hb@A%F@9�@57L@.Ta@('R@$oi@ 4n@�<@d�@S�@o�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�7B�1B�7B�7B�7B�7B�=B�=B�7B�7B�7B�7B�=B�hB�{B�VB�DB�DB�DB�JB�PB�hB��B��B�B�!B�'B�'B�'B�?B�jB��B��B��B�
B�B�NB�B��B  BB+B
=B�B+B-B.B/B(�B�B!�B�BoB
=BhBbBbB\B%�B:^B;dB9XB6FB1'B-B(�B"�B�BhB\BVBJBB��B��B�TB��B��BB�LB�B�bB�B�B~�B� Bw�Bp�Bm�Bl�BffBcTBr�B�PB�\B�B}�B|�Bz�Bp�B_;BO�BH�B9XB,B�BbB��B�mB��B��B�%Br�B^5BR�BA�B(�B\B
�B
�ZB
�BB
�B
�?B
�DB
n�B
ZB
P�B
G�B
;dB
/B
�B
{B
bB
	7B	��B	�/B	��B	�-B	��B	��B	��B	�hB	s�B	[#B	A�B	,B	hB	B��B�B�B�B�sB�HB�B��B��BĜB�wB�^B�?B�B�B��B��B��B�oB�\B�=B�%B~�Bz�Bx�Bx�Bw�Bv�Bv�Bu�Bu�Bt�Bt�Bs�Br�Br�Bo�Bn�Bn�Bk�Bl�Bl�Br�Bw�Bz�B}�B�B�=B�hB�{B��B�hB�JB�JB�oB��B��B�LBÖBɺB��B��B��B��B��B�
B�B�B�B��B��B��B�qB�?B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�\B�VB�PB�DB�DB�=B�1B�+B�+B�%B�B�B}�Bw�Br�Bn�Bk�BhsBbNB[#BXBXBXBZB[#B[#B\)B]/B^5B`BB`BB_;B_;B`BBbNBcTBffBe`BgmBgmBjBr�Bt�Bx�B|�B� B� B�B�%B�=B�DB�PB�hB�uB�{B��B��B��B��B��B��B��B��B�B�!B�'B�9B�LB�dB�wB�}B�wB�qB��B��BÖBǮB��B��B��B��B�B�B�B�)B�5B�`B�mB�B�B�B�B��B��B	B	%B	1B	PB	oB	{B	�B	�B	�B	�B	"�B	&�B	+B	.B	33B	33B	49B	6FB	9XB	<jB	>wB	A�B	B�B	E�B	H�B	I�B	I�B	I�B	J�B	J�B	J�B	L�B	M�B	O�B	O�B	S�B	W
B	XB	YB	[#B	YB	ZB	\)B	]/B	]/B	^5B	_;B	bNB	gmB	jB	k�B	l�B	o�B	s�B	t�B	u�B	u�B	v�B	x�B	y�B	y�B	y�B	z�B	{�B	|�B	}�B	�B	�B	�+B	�1B	�1B	�7B	�PB	�hB	�oB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�B	�B	�B	�!B	�3B	�LB	�qB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�)B	�/B	�/B	�5B	�5B	�5B	�;B	�;B	�BB	�HB	�HB	�TB	�fB	�fB	�mB	�sB	�B	�lB
�B
(B
�B
"�B
'mB
.B
2|B
;�B
CaB
G�B
NB
S�B
X+B
\xB
bB
ffB
j�B
o B
sh111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�(B�'B�(B�'B�)B�'B�2B�/B�)B�)B�'B�&B�4B�YB�pB�HB�:B�9B�4B�=B�AB�XB��B��B��B�B�B�B�B�2B�\B˸B��B��B��B�B�?B�B��B��BBB
.ByB*�B,�B.B/	B(�B~B!�B�BcB
/BYBSBTBOB%�B:NB;VB9HB68B1B,�B(�B"�BBYBOBFB?B�B��B��B�FB��B˷BB�:B��B�TB�B��B~�B�Bw�Bp�Bm�Bl}BfWBcCBr�B�BB�OB��B}�B|�Bz�Bp�B_-BO�BH�B9JB+�B�BRB��B�\BʳB��B�Br�B^&BR�BAxB(�BMB
�B
�LB
�0B
�B
�2B
�6B
n�B
ZB
P�B
G�B
;TB
/B
�B
lB
QB
	&B	��B	�B	��B	�B	��B	��B	��B	�ZB	s�B	[B	AwB	+�B	XB	 �B��B�B�B�B�cB�6B�
B��BʮBċB�eB�MB�.B�B��B��B��B�qB�]B�GB�*B�B~�Bz�Bx�Bx�Bw�Bv�Bv�Bu�Bu�Bt�Bt�Bs�Br�Br�Bo�Bn�Bn�BkrBlxBlyBr�Bw�Bz�B}�B�B�)B�WB�jB�oB�TB�8B�8B�\B��B��B�9BÄBɦB��B��B��B��B��B��B��B��B��B��B˵B�wB�]B�,B�B��B��B��B��B��B��B��B��B��B�lB��B��B��B�xB��B��B�yB�fB�TB�FB�EB�<B�2B�0B�(B�B�B�B�B�B��B}�Bw�Br�Bn�BksBhaBb;B[BW�BW�BW�BZ
B[B[B\B]B^ B`.B`.B_'B_&B`.Bb:Bc@BfPBeMBgYBgYBjiBr�Bt�Bx�B|�B�B�B�B�B�(B�/B�9B�RB�`B�gB�tB�B��B��B��B��B��B��B��B�B�B�$B�6B�RB�bB�fB�bB�[B�pB�oBÂBǛB˰BͿB��B��B��B�B�
B�B�B�KB�\B�wB�B�B�B��B��B	�B	B	B	:B	XB	fB	lB	vB	�B	�B	"�B	&�B	*�B	-�B	3B	3B	4$B	61B	9CB	<ZB	>cB	AuB	B{B	E�B	H�B	I�B	I�B	I�B	J�B	J�B	J�B	L�B	M�B	O�B	O�B	S�B	V�B	W�B	Y B	[B	YB	ZB	\B	]B	]B	^B	_(B	b8B	gZB	jkB	kmB	lwB	o�B	s�B	t�B	u�B	u�B	v�B	x�B	y�B	y�B	y�B	z�B	{�B	|�B	}�B	��B	�
B	�B	�B	�B	�"B	�;B	�UB	�YB	�YB	�YB	�^B	�hB	�uB	�sB	�yB	�wB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�9B	�]B	ōB	ɤB	ʭB	˲B	˲B	̷B	ͽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�"B	�$B	�$B	�/B	�0B	�3B	�?B	�QB	�RB	�WG�O�B	� B	�YB
�B
B
�B
"�B
'XB
-�B
2hB
;�B
CJB
G�B
M�B
S{B
XB
\bB
bB
fQB
j�B
n�B
sS111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.16 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             202008311646172020083116461720200831164617  AO  ARCAADJP                                                                    20190219181631    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20190219181631  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20190219181631  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20200831164617  IP                  G�O�G�O�G�O�                