CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-02-19T18:16:39Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190219181639  20200831164638  5903273 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL                A   AO  3334                            2C  D   APEX                            4917                            041310                          846 @ղ��&iQ1   @ղ�`�f@6��hr��cbI�^51   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                     A   B   B   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>y�D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dt� Dy��D��D�NfD��RD��{D� �D�8 D�t)D��qD�D�<{D��{D�ǮD�\D�<)DڍD�)D��D�9�D�x D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @>{@�p�@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�\)A�(�A�\)B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�W
B�W
B�W
B��=B�W
B�W
B�W
B�W
B�W
B�W
B��=B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C +�C+�C+�C+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
�D ��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D	
�D	��D

�D
��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D 
�D ��D!
�D!��D"
�D"��D#
�D#��D$
�D$��D%
�D%��D&
�D&��D'
�D'��D(
�D(��D)
�D)��D*
�D*��D+
�D+��D,
�D,��D-
�D-��D.
�D.��D/
�D/��D0
�D0��D1
�D1��D2
�D2��D3
�D3��D4
�D4��D5
�D5��D6
�D6��D7
�D7��D8
�D8��D9
�D9��D:
�D:��D;
�D;��D<
�D<��D=
�D=��D>
�D>�{D?
�D?��D@
�D@��DA
�DA��DB
�DB��DC
�DC��DD
�DD��DE
�DE��DF
�DF��DG
�DG��DH
�DH��DI
�DI��DJ
�DJ��DK
�DK��DL
�DL��DM
�DM��DN
�DN��DO
�DO��DP
�DP��DQ
�DQ��DR
�DR��DS
�DS��DT
�DT��DU
�DU��DV
�DV��DW
�DW��DX
�DX��DY
�DY��DZ
�DZ��D[
�D[��D\
�D\��D]
�D]��D^
�D^��D_
�D_��D`
�D`��Da
�Da��Db
�Db��Dc
�Dc��Dd
�Dd��De
�De��Df
�Df��Dg
�Dg��Dh
�Dh��Di
�Di��Dj
�Dj��Dk
�Dk��Dl
�Dl��Dm
�Dm��Dn
�Dn��Do
�Do��Dp
�Dp��Dq
�Dq��Dr
�Dr��Ds
�Ds��Dt{Dt��Dy��D�	HD�S�D���D���D�gD�=qD�y�D���D��D�A�D���D��D��D�A�Dڒ�DౚD�
>D�?]D�}qD��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�dZA�\)A�XA�O�A�G�A�C�A�9XA�=qA�?}A�9XA�7LA�7LA�7LA�7LA�5?A�33A�5?A�/A�+A�+A�+A�-A�-A�/A�/A�(�A�-A�/A�+A�+A�+A�/A�1'A�1'A�33A�33A�/A�+A�(�A��A��mA�ZA�M�A�/A��A�VA�"�A�=qA��!A���A��hA��FA�oA���A�dZA��\A�ĜA��A�hsA�K�A���A���A�dZA��
A�JA�XA���A�E�A���A��A��9A�/A�7LA��hA��/A���A���A���A�oA��A�ĜA�O�A�x�A�O�A�oA��A�G�A�oA��!A�JA�t�A�VA�z�A�^5A�n�A���A���A�A�A��A�1A�O�A�XA�C�A�z�A���A���A�JA�K�A���A�G�A�K�A��A���A�VA��7A��A�5?A��wA�7LA���A��!A~�uA|�Ay�;Av�\AuC�AtE�ApjAn��Al��Aj��Ai��Ait�Ag�
Ac�AadZA`�!A_��A\�AZbNAW�TAV�9AV(�AQ�mAOp�AM��AL�\AKK�AJ1AGO�AE`BAD(�ABv�AA�wAA��AA��A@�A>VA<��A;��A:�jA;�A:��A9�#A8�yA7C�A6jA4�RA2�`A2E�A1K�A0�A/��A/�A/K�A.�yA.�A-A,ZA+��A*I�A)l�A(�`A'�A&��A%��A$�A$�\A$-A#%A"n�A!�A!��A!
=A��A+Av�Ap�AJAp�A��AhsA��At�Az�AƨAoA�hA^5A1AȴAG�A�A�/A�RA(�A�A
�!A
bNA	�A	x�A	/AA��AJA��A�;Ax�A��A�A��A�A�uA  A��A�A 5?@���@�Ĝ@�l�@�1'@�X@��;@�ff@��@�|�@�C�@�R@���@���@���@�ȴ@��@���@��;@�C�@�{@�G�@�j@�@��@�A�@�n�@���@�?}@�  @��@У�@�+@�ff@��#@ͩ�@�O�@̃@��@�K�@�v�@��@ɲ-@Ȭ@�1@�;d@�5?@��/@Ý�@�@��@��@���@��;@�K�@�;d@���@��^@��@�9X@��w@��y@�J@�&�@�33@�Z@��#@���@��@���@�+@���@��@�(�@��
@�v�@���@��@�X@��@��j@��j@��@��`@���@�G�@��@��@�$�@���@�?}@���@�Q�@�ƨ@�\)@��H@�n�@���@��7@�X@�/@���@��`@���@�  @�+@���@��\@�ff@�M�@�5?@���@��@��^@��7@��@��h@�x�@�p�@�G�@�j@�1@��@��@�S�@�+@��H@���@�n�@�5?@���@��7@�p�@�X@�/@���@�r�@� �@���@���@�|�@�l�@�S�@�33@�+@�
=@���@�ff@�5?@��@���@���@��-@�p�@���@��@��u@��D@��@�r�@�1@��@��m@��w@�|�@�@���@�~�@�-@�@��@��^@�x�@���@���@�z�@�Q�@� �@���@��P@�dZ@�+@��@��R@��\@�ff@�=q@���@�x�@�X@�/@�%@���@��j@�j@�(�@��@��
@���@�ƨ@��w@���@�|�@�+@���@�V@�5?@��@�@�@���@��h@��@��`@�Ĝ@���@�z�@�I�@�b@�ƨ@���@��P@��y@���@�~�@�ff@�V@�$�@�$�@���@�@�`B@�G�@�/@��9@���@��u@�r�@���@�l�@�K�@�o@��H@���@��+@�ff@�M�@�$�@�J@���@�&�@��`@�Ĝ@��@}4@s�@j��@c��@\��@T$@MJ�@E��@@�I@:E�@6YK@/J#@(	�@$!@��@�~@�Q@b�@��@�z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�dZA�\)A�XA�O�A�G�A�C�A�9XA�=qA�?}A�9XA�7LA�7LA�7LA�7LA�5?A�33A�5?A�/A�+A�+A�+A�-A�-A�/A�/A�(�A�-A�/A�+A�+A�+A�/A�1'A�1'A�33A�33A�/A�+A�(�A��A��mA�ZA�M�A�/A��A�VA�"�A�=qA��!A���A��hA��FA�oA���A�dZA��\A�ĜA��A�hsA�K�A���A���A�dZA��
A�JA�XA���A�E�A���A��A��9A�/A�7LA��hA��/A���A���A���A�oA��A�ĜA�O�A�x�A�O�A�oA��A�G�A�oA��!A�JA�t�A�VA�z�A�^5A�n�A���A���A�A�A��A�1A�O�A�XA�C�A�z�A���A���A�JA�K�A���A�G�A�K�A��A���A�VA��7A��A�5?A��wA�7LA���A��!A~�uA|�Ay�;Av�\AuC�AtE�ApjAn��Al��Aj��Ai��Ait�Ag�
Ac�AadZA`�!A_��A\�AZbNAW�TAV�9AV(�AQ�mAOp�AM��AL�\AKK�AJ1AGO�AE`BAD(�ABv�AA�wAA��AA��A@�A>VA<��A;��A:�jA;�A:��A9�#A8�yA7C�A6jA4�RA2�`A2E�A1K�A0�A/��A/�A/K�A.�yA.�A-A,ZA+��A*I�A)l�A(�`A'�A&��A%��A$�A$�\A$-A#%A"n�A!�A!��A!
=A��A+Av�Ap�AJAp�A��AhsA��At�Az�AƨAoA�hA^5A1AȴAG�A�A�/A�RA(�A�A
�!A
bNA	�A	x�A	/AA��AJA��A�;Ax�A��A�A��A�A�uA  A��A�A 5?@���@�Ĝ@�l�@�1'@�X@��;@�ff@��@�|�@�C�@�R@���@���@���@�ȴ@��@���@��;@�C�@�{@�G�@�j@�@��@�A�@�n�@���@�?}@�  @��@У�@�+@�ff@��#@ͩ�@�O�@̃@��@�K�@�v�@��@ɲ-@Ȭ@�1@�;d@�5?@��/@Ý�@�@��@��@���@��;@�K�@�;d@���@��^@��@�9X@��w@��y@�J@�&�@�33@�Z@��#@���@��@���@�+@���@��@�(�@��
@�v�@���@��@�X@��@��j@��j@��@��`@���@�G�@��@��@�$�@���@�?}@���@�Q�@�ƨ@�\)@��H@�n�@���@��7@�X@�/@���@��`@���@�  @�+@���@��\@�ff@�M�@�5?@���@��@��^@��7@��@��h@�x�@�p�@�G�@�j@�1@��@��@�S�@�+@��H@���@�n�@�5?@���@��7@�p�@�X@�/@���@�r�@� �@���@���@�|�@�l�@�S�@�33@�+@�
=@���@�ff@�5?@��@���@���@��-@�p�@���@��@��u@��D@��@�r�@�1@��@��m@��w@�|�@�@���@�~�@�-@�@��@��^@�x�@���@���@�z�@�Q�@� �@���@��P@�dZ@�+@��@��R@��\@�ff@�=q@���@�x�@�X@�/@�%@���@��j@�j@�(�@��@��
@���@�ƨ@��w@���@�|�@�+@���@�V@�5?@��@�@�@���@��h@��@��`@�Ĝ@���@�z�@�I�@�b@�ƨ@���@��P@��y@���@�~�@�ff@�V@�$�@�$�@���@�@�`B@�G�@�/@��9@���@��u@�r�@���@�l�@�K�@�o@��H@���@��+@�ff@�M�@�$�@�J@���@�&�@��`G�O�@��@}4@s�@j��@c��@\��@T$@MJ�@E��@@�I@:E�@6YK@/J#@(	�@$!@��@�~@�Q@b�@��@�z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBbBbBbBbB\B\B\B\B\B\B\B\B\B\B\BVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBVBJB1BB��B��B�B�B�B��BB  B��B��B��B��BPBJBbB+B�B�mB�;B�TB�5B�
B�B�;B��B+B%BB+B1BB��B��B�B�BB�B{BPBB�B��B��B��B�dB��B�7B� Bs�B\)BO�BW
B`BBr�Bt�BjBS�BB�B5?B �B%B�NB��B�FB��B�Bo�BXBD�B5?B�B  B
�B
�TB
�B
�}B
��B
�1B
n�B
W
B
B�B
,B
bB
B	��B	�HB	�
B	ǮB	�^B	�-B	�B	��B	�B	�B	�B	z�B	\)B	A�B	-B	0!B	33B		7B�B�`B�NB�B�B��B�NB�ZB�;B�/B�;B�`B�BB��BŢBÖBƨB��B�B��B��BǮBŢB�XB�'B�LB�?B�3B�-B�-B�'B�!B��B��B��B��B��B��B��B��B�uB�VB�JB�DB�7B�+B�B�B~�By�Bo�BiyBgmBe`BaHB^5B[#BW
BS�BP�BN�BL�BJ�BG�BE�BD�BB�BB�BA�BB�BC�BD�BB�BA�BA�B@�B?}B>wB;dB9XB8RB8RB7LB7LB6FB7LB8RB8RB8RB8RB7LB6FB7LB8RB8RB7LB6FB6FB5?B6FB7LB5?B6FB6FB49B49B5?B7LB:^B@�BB�BB�BD�BE�BE�BF�BF�BE�BF�BF�BE�BE�BB�B@�BA�BB�BC�BB�BA�BB�BB�BA�BA�BA�BA�BB�BB�BC�BE�BH�BJ�BK�BM�BM�BO�BR�BW
B\)B^5BaHBbNBdZBdZBdZBdZBe`Be`BaHB]/B\)BaHBhsBhsBffBjBm�Bm�Bn�Bp�Bp�Bq�Bt�Bv�B}�B�+B��B��B��B��B��B��B�B�'B�9B�LB�^B�wB��BŢB��B��B��B��B��B�B�
B�#B�TB�fB�yB�B�B�B�B��B��B��B��B	  B	B	B	B	B	1B		7B	DB	\B	hB	{B	�B	�B	�B	%�B	(�B	+B	-B	.B	0!B	6FB	:^B	>wB	@�B	A�B	B�B	C�B	D�B	D�B	E�B	J�B	M�B	N�B	P�B	Q�B	S�B	T�B	XB	]/B	aHB	cTB	dZB	dZB	e`B	iyB	iyB	jB	k�B	l�B	q�B	t�B	u�B	w�B	x�B	y�B	{�B	|�B	�B	�B	�B	�%B	�1B	�DB	�PB	�VB	�hB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�-B	�3B	�FB	�FB	�FB	�LB	�RB	�XB	�XB	�dB	�jB	�wB	�wB	�}B	��B	B	ŢB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�)B	�5B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�NB	�TB	�ZB	�`B	�mB	��B	�B
B
�B
"hB
(�B
-�B
49B
8B
?HB
DB
G�B
M6B
T,B
Y1B
^�B
a�B
g�B
j�B
n/B
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BSBSBSBSBOBOBOBOBQBQBOBOBOBQBOBFBFBFBFBHBFBHBHBHBFBFBHBHBFBHBHBFBFBFBCBHBFBFBHB;B#B	B��B��B�B�B�B��BB��B��B��B��B��BBB>BRBB�B�`B�,B�GB�&B��B��B�*B��BBBBB!BB��B��B�B�BB�BiBBBB�B��B��B��B�SB��B�&B�Bs�B\BO�BV�B`7Br�Bt�BjrBS�BB�B5.B �BB�?B��B�4B��B��Bo�BX BD�B51B�B
��B
�B
�DB
�B
�iB
��B
�"B
n�B
V�B
B{B
+�B
RB
B	��B	�6B	��B	ǜB	�MB	�B	��B	��B	�B	�B	��B	z�B	\B	AyB	,�B	0B	3 B		%B�B�PB�;B��B��B��B�;B�GB�(B�B�(B�MB�.B��BőBÄBƔB��B��B��B��BǛBŐB�FB�B�:B�,B�!B�B�B�B�B��B��B��B��B��B��B��B��B�aB�BB�6B�0B�$B�B�B��B~�By�Bo�BieBgWBeLBa5B^"B[BV�BS�BP�BN�BL�BJ�BG�BE�BD�BB|BB}BAuBB{BC�BD�BBzBAsBAtB@mB?iB>bB;OB9BB8=B8>B79B79B62B75B8;B8>B8;B8>B77B6/B78B8=B8>B77B62B6/B5)B61B7:B5+B60B62B4"B4$B5*B77B:HB@nBByBByBD�BE�BE�BF�BF�BE�BF�BF�BE�BE�BBxB@oBAtBB{BC�BBxBAsBBxBBwBAtBAsBAsBAsBBwBByBC�BE�BH�BJ�BK�BM�BM�BO�BR�BV�B\B^ Ba1Bb9BdCBdCBdBBdEBeJBeJBa4B]B\Ba1BhZBh]BfQBjiBm|BmzBn�Bp�Bp�Bq�Bt�Bv�B}�B�B�qB�|B��B��B��B��B��B�B�$B�4B�HB�aB�tBōBʪBͺB��B��B��B��B��B�B�=B�OB�dB�pB�{B�B��B��B��B��B��B��B	�B	�B	�B		B	B		 B	0B	DB	RB	cB	~B	�B	�B	%�B	(�B	*�B	,�B	-�B	0B	60B	:IB	>_B	@lB	ApB	BwB	C�B	D�B	D�B	E�B	J�B	M�B	N�B	P�B	Q�B	S�B	T�B	W�B	]B	a2B	c>B	dAB	dDB	eKB	ibB	iaB	jiB	knB	luB	q�B	t�B	u�B	w�B	x�B	y�B	{�B	|�B	��B	�B	�	B	�B	�B	�,B	�:B	�?B	�PB	�RB	�fB	�oB	�xB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�0B	�/B	�.B	�7B	�;B	�@B	�AB	�MB	�SB	�_B	�aB	�gB	�tB	�zB	ŋB	ŉB	ŌB	ȚB	ʩB	˲B	̶B	ͻB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�$B	�(B	�-B	�3B	�1B	�7B	�8B	�;B	�>B	�CB	�HG�O�B	��B	��B
B
�B
"TB
(�B
-�B
4$B
7�B
?2B
DB
G�B
MB
TB
YB
^�B
a�B
g�B
j�B
nB
q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             202008311646382020083116463820200831164638  AO  ARCAADJP                                                                    20190219181639    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20190219181639  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20190219181639  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20200831164638  IP                  G�O�G�O�G�O�                