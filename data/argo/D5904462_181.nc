CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:57Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125957  20190405100801  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @����{ t1   @���8㭦@/~��"���c��S���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dys3D���D�C3D�� D��3D���D�@ D�s3D���D�3D�9�D��3D��3D�  D�@ D�vfD��fD� D�9�D�3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@�  A  A(  AH  Ah  A�  A�  A�  A�  A�  A�  A�  A�  B  B
  B  B  B"  B*  B2  B:  BB  BJ  BR  BZ  Bb  Bj  Br  Bz  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C � C� C� C� C� C
� C� C� C� C� C� C� C� C� C� C� C � C"� C$� C&� C(� C*� C,� C.� C0� C2� C4� C6� C8� C:� C<� C>� C@� CB� CD� CF� CH� CJ� CL� CN� CP� CR� CT� CV� CX� CZ� C\� C^� C`� Cb� Cd� Cf� Ch� Cj� Cl� Cn� Cp� Cr� Ct� Cv� Cx� Cz� C|� C~� C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt��Dy�3D���D�S3D�� D��3D��D�P D��3D���D�#3D�I�D��3D��3D� D�P DچfD��fD�  D�I�D�3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�I�A�1'A��TA���A�ĜAʹ9A͟�A͏\A�~�A�x�A�n�A�ffA�dZA�\)A�I�A� �A�A��`A̾wA̙�A̋DA�~�A�/A˺^A�ZAʩ�A�A�A�  Aǧ�Aǥ�AǓuA�ffA�A���A���A�?}A�%AƸRA�t�Aƙ�Aƥ�A�?}A�oA�VA�%A��A���Aƴ9AƝ�A�~�A�E�A��mA�G�A��`A��HA��A��A��\A���A��A���A��A�-A��
A��A���A�dZA�bNA��9A�"�A��A��A�bA��HA��A��PA���A���A�7LA��PA���A���A���A���A�~�A��A��+A��jA�A�A��mA�G�A���A�%A�A�\)A��A���A��mA�1A�?}A�E�A��yA���A�JA�p�A���A���A���A�FA{7LAxAuO�Aq��Am��Aj�uAf�jAd  A`�A]AXE�AV5?AT�AQ33AO%AJ��AH��AGdZAF�ADv�AB�RA@��A=hsA:  A8-A5C�A2�/A0M�A.��A-`BA,{A+�A)�#A*=qA)VA(ffA'�-A'A&-A%�TA%A%��A%dZA%oA$�RA$~�A$z�A#�
A#33A#S�A#%A"ĜA"I�A~�A�DAA�7A�;AE�A~�A  A(�A��A��A�`A��AffA�hA�HA�
A�AhsA�yAffAhsA�PA$�A
�HA	7LA$�A&�A�\A�wAhsA�A9XA�Al�A+A�\Av�A�!A�uA��Az�Az�An�A1A�AVA ff@�|�@���@�J@�J@�{@��@��u@�  @�"�@�J@�?}@���@���@��u@�A�@���@��+@��@��h@��@���@�1'@�dZ@�E�@�bN@�z�@��;@�ff@�Ĝ@�I�@�@�33@ꟾ@��@�@�\@�@�7L@�j@�F@�|�@�33@�J@�7L@��@� �@���@�5?@��`@�A�@ۍP@ڇ+@�$�@٩�@١�@ف@�X@؋D@׍P@�;d@�o@և+@�J@Ԭ@�9X@�33@�5?@щ7@�?}@�&�@�Q�@϶F@�K�@�C�@·+@Ͳ-@�/@��/@�z�@ˍP@�\)@�"�@ʏ\@�M�@Ɂ@�j@�Q�@�j@��
@��H@�~�@�E�@���@�@Ų-@�/@�/@��@ă@� �@��@��m@öF@öF@��@�{@��-@�O�@�%@�%@��@�z�@��@�|�@�ƨ@���@�C�@�33@��!@�J@�hs@�7L@��@���@�(�@� �@��@��R@�|�@��F@�\)@�33@�
=@��@���@�-@��@�x�@�?}@�V@��9@� �@���@�C�@��y@���@�M�@��@���@��@�&�@��@���@�ȴ@�V@��#@�7L@��`@�9X@���@�;d@��R@�M�@�ff@�ȴ@��!@�~�@��^@��`@�ƨ@�|�@�
=@���@�~�@�v�@��+@�
=@�"�@���@��@�J@�@��^@�p�@���@�Q�@�1@�ƨ@�K�@�C�@�v�@�/@���@�Ĝ@��@�1'@��;@��F@�"�@���@�n�@��h@�7L@�V@��`@�Z@� �@���@�;d@��@��H@��\@�{@�{@���@�O�@�Ĝ@��u@�bN@�Q�@�A�@��@�l�@���@���@���@�v�@�=q@�{@�@��@���@�O�@���@��9@�I�@� �@�b@�  @��m@��@�K�@�33@��@��@��P@�ƨ@�o@��H@��!@��\@�^5@�$�@���@��^@���@�O�@��@���@���@���@�r�@���@�K�@��H@��R@�ff@��h@�hs@�?}@��@�V@��@���@��u@�9X@�9X@�1@�ff@�l�@���@y�#@p��@f��@\��@Tj@K�
@Dz�@=�@65?@1X@*��@%?}@ b@^5@�y@"�@;d@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�I�A�1'A��TA���A�ĜAʹ9A͟�A͏\A�~�A�x�A�n�A�ffA�dZA�\)A�I�A� �A�A��`A̾wA̙�A̋DA�~�A�/A˺^A�ZAʩ�A�A�A�  Aǧ�Aǥ�AǓuA�ffA�A���A���A�?}A�%AƸRA�t�Aƙ�Aƥ�A�?}A�oA�VA�%A��A���Aƴ9AƝ�A�~�A�E�A��mA�G�A��`A��HA��A��A��\A���A��A���A��A�-A��
A��A���A�dZA�bNA��9A�"�A��A��A�bA��HA��A��PA���A���A�7LA��PA���A���A���A���A�~�A��A��+A��jA�A�A��mA�G�A���A�%A�A�\)A��A���A��mA�1A�?}A�E�A��yA���A�JA�p�A���A���A���A�FA{7LAxAuO�Aq��Am��Aj�uAf�jAd  A`�A]AXE�AV5?AT�AQ33AO%AJ��AH��AGdZAF�ADv�AB�RA@��A=hsA:  A8-A5C�A2�/A0M�A.��A-`BA,{A+�A)�#A*=qA)VA(ffA'�-A'A&-A%�TA%A%��A%dZA%oA$�RA$~�A$z�A#�
A#33A#S�A#%A"ĜA"I�A~�A�DAA�7A�;AE�A~�A  A(�A��A��A�`A��AffA�hA�HA�
A�AhsA�yAffAhsA�PA$�A
�HA	7LA$�A&�A�\A�wAhsA�A9XA�Al�A+A�\Av�A�!A�uA��Az�Az�An�A1A�AVA ff@�|�@���@�J@�J@�{@��@��u@�  @�"�@�J@�?}@���@���@��u@�A�@���@��+@��@��h@��@���@�1'@�dZ@�E�@�bN@�z�@��;@�ff@�Ĝ@�I�@�@�33@ꟾ@��@�@�\@�@�7L@�j@�F@�|�@�33@�J@�7L@��@� �@���@�5?@��`@�A�@ۍP@ڇ+@�$�@٩�@١�@ف@�X@؋D@׍P@�;d@�o@և+@�J@Ԭ@�9X@�33@�5?@щ7@�?}@�&�@�Q�@϶F@�K�@�C�@·+@Ͳ-@�/@��/@�z�@ˍP@�\)@�"�@ʏ\@�M�@Ɂ@�j@�Q�@�j@��
@��H@�~�@�E�@���@�@Ų-@�/@�/@��@ă@� �@��@��m@öF@öF@��@�{@��-@�O�@�%@�%@��@�z�@��@�|�@�ƨ@���@�C�@�33@��!@�J@�hs@�7L@��@���@�(�@� �@��@��R@�|�@��F@�\)@�33@�
=@��@���@�-@��@�x�@�?}@�V@��9@� �@���@�C�@��y@���@�M�@��@���@��@�&�@��@���@�ȴ@�V@��#@�7L@��`@�9X@���@�;d@��R@�M�@�ff@�ȴ@��!@�~�@��^@��`@�ƨ@�|�@�
=@���@�~�@�v�@��+@�
=@�"�@���@��@�J@�@��^@�p�@���@�Q�@�1@�ƨ@�K�@�C�@�v�@�/@���@�Ĝ@��@�1'@��;@��F@�"�@���@�n�@��h@�7L@�V@��`@�Z@� �@���@�;d@��@��H@��\@�{@�{@���@�O�@�Ĝ@��u@�bN@�Q�@�A�@��@�l�@���@���@���@�v�@�=q@�{@�@��@���@�O�@���@��9@�I�@� �@�b@�  @��m@��@�K�@�33@��@��@��P@�ƨ@�o@��H@��!@��\@�^5@�$�@���@��^@���@�O�@��@���@���@���@�r�@���@�K�@��H@��R@�ff@��h@�hs@�?}@��@�V@��@���@��u@�9X@�9X@�1@�ff@�l�@���@y�#@p��@f��@\��@Tj@K�
@Dz�@=�@65?@1X@*��@%?}@ b@^5@�y@"�@;d@t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBZBYBYBYBXBXBXBXBW
BW
BVBVBW
B\)BbNBjBo�Bs�Bz�B�%B�PB�bB�BƨB�;B	DB	C�B	gmB	z�B	}�B	�B	�{B	��B	��B	�LB	�B	�/B	�B	�B
"�B
E�B
�B
�dB
�BbB5?B<jBVBx�B�B�oB��B��B��B��B�BƨB�wB�dBȴB�sBBBDB{BPB  B�B�`B�#B��B��B�LB�^B��B�RB��B�bB�?B�;BŢB�B�hB�B�B�\Bz�BS�BK�BF�B:^B�B
��B
�B
�BB
��B
�-B
��B
�PB
� B
t�B
gmB
XB
P�B
I�B
A�B
>wB
33B
)�B
B	�sB	�B	ƨB	�-B	��B	�JB	x�B	dZB	T�B	<jB	33B	-B	 �B	uB��B�B�sB�NB�B�BB�;B�BB�NB�TB�TB�)B�;B�ZB�yB�B��B	B	&�B	'�B	&�B	+B	6FB	B�B	E�B	J�B	T�B	[#B	aHB	iyB	m�B	t�B	z�B	�%B	�\B	��B	��B	��B	�1B	}�B	~�B	�B	�VB	��B	��B	�B	ÖB	B	�^B	�B	��B	��B	��B	��B	��B	�hB	�DB	�%B	� B	w�B	k�B	aHB	XB	L�B	C�B	:^B	6FB	6FB	:^B	;dB	>wB	B�B	C�B	F�B	I�B	M�B	S�B	YB	cTB	e`B	e`B	e`B	hsB	k�B	m�B	m�B	n�B	p�B	v�B	x�B	z�B	{�B	~�B	�B	�B	�7B	�JB	�PB	�PB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�!B	�9B	�9B	�9B	�9B	�9B	�?B	�FB	�RB	�RB	�RB	�XB	�dB	�jB	�jB	�jB	�jB	�qB	�qB	�wB	�}B	�}B	�}B	�qB	�jB	�jB	�^B	�XB	�XB	�RB	�LB	�FB	�?B	�RB	�XB	�dB	�^B	�^B	�qB	�jB	�wB	��B	��B	B	B	ÖB	ĜB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�BB	�HB	�ZB	�fB	�B	�B	�sB	�yB	�sB	�yB	�B	�B	�B	�yB	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
  B
B
B
B
B
B
B
B
B
B
  B
B
%B
B
B
B
B
B
B
B
B
+B
+B
%B
B
B
B
B
B
B
B
+B
1B
DB
PB
JB
PB
VB
PB
VB
VB
VB
VB
VB
VB
PB
VB
PB
	7B
	7B
	7B
	7B

=B
DB

=B
	7B

=B

=B
1B
1B
1B
	7B
1B
1B
+B
1B
1B

=B
DB
DB
VB
\B
VB
VB
VB
VB
VB
\B
bB
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
!�B
#�B
#�B
'�B
/B
8RB
>wB
C�B
I�B
L�B
S�B
ZB
]/B
cTB
e`B
gmB
k�B
o�B
v�B
w�B
y�B
~�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BY�BX�BX�BX�BW�BW�BW�BW�BV�BV�BU�BU�BV�B\Bb&BjYBouBs�Bz�B��B�(B�;B��B�B�B	B	CmB	gFB	z�B	}�B	��B	�VB	��B	��B	�!B	��B	�B	�\B	�B
"�B
EvB
��B
�>B
��B8B5B<?BU�Bx�B��B�GB��B��B��B��B��B�|B�LB�:BȋB�IB �B�BBNB#B��B�yB�5B��BΫB�VB�B�3BΫB�%B��B�6B�B�B�vB��B�6B��B��B�+Bz�BS�BK�BFyB:0B�B
��B
�~B
�B
�YB
��B
��B
�B
�B
t�B
g:B
W�B
P�B
I�B
AUB
>CB
3B
)�B
�B	�@B	��B	�uB	��B	��B	�B	x�B	d$B	T�B	<3B	2�B	,�B	 �B	=B��B�nB�<B�B��B�
B�B�	B�B�B�B��B�B�!B�@B�qB��B	�B	&�B	'�B	&�B	*�B	6B	BTB	EhB	J�B	T�B	Z�B	aB	i>B	mXB	t�B	z�B	��B	�!B	�XB	�fB	�`B	��B	}�B	~�B	��B	�B	�WB	��B	��B	�ZB	�TB	�#B	��B	��B	��B	�xB	�_B	�RB	�-B	�	B	��B	�B	w�B	kIB	aB	W�B	L�B	C[B	:#B	6	B	6	B	:"B	;%B	><B	BRB	CYB	FiB	IB	M�B	S�B	X�B	cB	e&B	e&B	e"B	h6B	kHB	mTB	mSB	n\B	peB	v�B	x�B	z�B	{�B	~�B	��B	��B	��B	�B	�B	�B	�%B	�9B	�=B	�ZB	�\B	�]B	�]B	�cB	�qB	�tB	�\B	�bB	�tB	��B	��B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�$B	�,B	�+B	�+B	�)B	�2B	�1B	�8B	�?B	�?B	�?B	�2B	�,B	�)B	�"B	�B	�B	�B	�B	�B	�B	�B	�B	�$B	� B	�B	�2B	�+B	�8B	�BB	�HB	�OB	�OB	�SB	�[B	�B	ˆB	ʂB	ΘB	ΚB	ХB	ԾB	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�=B	�=B	�5B	�9B	�6B	�9B	�>B	�KB	�=B	�8B	�2B	�WB	�gB	�pB	�{B	�{B	��B	�|B	��B	��B	��B	��B	��B	�zB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
 �B	��B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
B
B
	B
B
B
B
B
B
B
B
B
B
B
B
B
�B
�B
�B
�B
	�B
B
	�B
�B
	�B
	�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
B
B
B
B
B
B
B
B
B
B
!B
!B
!B
!B
"B
B
 B
'B
'B
%B
$B
'B
)B
0B
.B
-B
/B
1B
2B
2B
9B
;B
9B
EB
]B
jB
lB
hB
oB
iB
iB
jB
rB
sB
oB
rB
sB
sB
rB
qB
pB
kB
fB
eB
fB
jB
qB
rB
uB
vB
{B
 �B
 �B
 �B
!�B
!�B
#�B
#�B
'�B
.�B
8B
>5B
CSB
I{B
L�B
S�B
Y�B
\�B
cB
eB
g+B
kCB
o\B
v�B
w�B
y�B
~�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.5 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008012019040510080120190405100801  AO  ARCAADJP                                                                    20181121125957    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125957  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125957  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100801  IP                  G�O�G�O�G�O�                