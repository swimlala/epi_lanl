CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:46Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125946  20190405100754  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��T�m1   @��T�[�@/��
=q�d��hr�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C �C�C�fC�fC�fC
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dys3D� D�<�D�vfD���D�3D�S3D�y�D���D�fD�C3D���D�� D� D�@ D�vfD���D�3D�P D�s3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�\B�\B�\B�\B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�C �HC�HCnCnCnC
��C��C��C��C��C��CnC��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>�HC@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv�HCx�HCz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&�RD'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt�Dy�D� �D�M�D��\D���D�$)D�d)D���D���D�'\D�T)D���D���D� �D�P�Dڇ\D���D�$)D�`�D�)D�ʐ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A��
A��
A��A��
A���A���A���A���A��
A��A��
A���A�ĜA���A�ĜA�\A��A�A�-A㙚A�?}A���A�dZA�XA�A�A� �A��A�wA�|�A�$�A�  A�A�O�A�C�A�`BA��mA�\)Aݟ�A��A�9XA���A١�A�\)A�5?A�\)A���A��AэPA�dZA�M�A��mA���AρA��;A�VA�|�A�ffA���A�-A�ȴA�G�A�VA���Aě�AîA�A��A��PA��A��`A�&�A�t�A�C�A�bNA�VA���A�bNA���A�z�A���A��A���A�bA��TA���A�x�A�bNA�;dA�5?A��A�dZA���A�/A���A�&�A��A�jA�Azr�AsS�An��Aj��Ah�HAh1'Ag�AeXAc�A`^5A[�AX�HAT�AM��AJ��AJ�DAG��AEx�AD��AD��AD5?AC��AB�DA>��A="�A<�RA<1'A;��A;A9;dA7��A5�^A4z�A2A�A1�A.��A,n�A+A*��A'�A%�A#��A#�A"A�A!"�A ĜA  �A��AȴAAp�A;dA%Az�A1A�A�/A�DAVA1'A�mA��A��At�AXA
=A�DA=qA$�A�A��AdZA�`A�AhsA�\AA��A��A=qAƨA��A�wA��A�;A�A�A�A��A��AQ�A�A;dA
=A�HA�9A��A��A��A��A
�A
-A	7LA��AoA�A��A�A �jA -@�
=@�O�@��@�bN@�(�@��m@�K�@��@���@�`B@��@���@��u@��F@��y@�ff@�=q@�$�@��@�@��#@��-@��-@���@��^@�F@�p�@�b@�\)@��@�/@���@�ƨ@��@��/@�l�@�33@���@�7L@���@�"�@�n�@�J@�J@ᙚ@�%@�@�1'@�+@���@�M�@�V@��;@��@�|�@�v�@�5?@�5?@�J@ݑh@�Z@ۥ�@�@�E�@�M�@��@�x�@�p�@�/@؛�@�Z@�(�@��;@�K�@ָR@�J@�~�@�v�@�G�@��/@�9X@��y@�@щ7@�?}@�`B@�x�@с@�S�@��@�`B@�%@�V@͡�@͙�@�%@̴9@�bN@�b@�+@ʗ�@��@�~�@�E�@�J@��T@���@ɲ-@�hs@�V@�j@�l�@�33@�l�@�;d@�ȴ@�5?@Ų-@ũ�@�O�@ļj@�Z@� �@Õ�@�~�@�@���@��@�Q�@��@�ƨ@�C�@���@��#@���@�j@��@��w@���@�C�@��@��@���@���@�@��@���@��+@�^5@�{@���@�hs@���@�9X@��@�t�@�"�@�
=@��H@���@��@���@�X@���@�z�@�1'@� �@�ƨ@�dZ@��\@�hs@�&�@��`@��@�Q�@�I�@���@�ƨ@���@�;d@�^5@��h@�/@���@���@��@�j@�Q�@�Z@�1'@��@��@�ƨ@�t�@��H@��+@��T@��-@�/@��@�I�@��@���@���@�\)@�;d@�"�@�o@��y@���@��#@���@�x�@�hs@�`B@�7L@���@��9@��@�Z@��w@��P@�o@��+@�-@��@���@���@�p�@�/@�V@���@��@��`@���@�bN@� �@�ƨ@�33@��@�V@��@��`@���@�bN@�I�@�A�@�9X@��@��@��w@�l�@�+@���@�~�@�5?@��@�/@��9@�r�@�A�@�ƨ@�|�@�dZ@�\)@��@��!@��+@�E�@�J@�p�@�/@�&�@�%@���@���@�o@��7@���@{��@r~�@fV@[�F@TI�@MV@D�j@=p�@4(�@-�@%�@K�@��@\)@9X@X@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A��
A��
A��A��
A���A���A���A���A��
A��A��
A���A�ĜA���A�ĜA�\A��A�A�-A㙚A�?}A���A�dZA�XA�A�A� �A��A�wA�|�A�$�A�  A�A�O�A�C�A�`BA��mA�\)Aݟ�A��A�9XA���A١�A�\)A�5?A�\)A���A��AэPA�dZA�M�A��mA���AρA��;A�VA�|�A�ffA���A�-A�ȴA�G�A�VA���Aě�AîA�A��A��PA��A��`A�&�A�t�A�C�A�bNA�VA���A�bNA���A�z�A���A��A���A�bA��TA���A�x�A�bNA�;dA�5?A��A�dZA���A�/A���A�&�A��A�jA�Azr�AsS�An��Aj��Ah�HAh1'Ag�AeXAc�A`^5A[�AX�HAT�AM��AJ��AJ�DAG��AEx�AD��AD��AD5?AC��AB�DA>��A="�A<�RA<1'A;��A;A9;dA7��A5�^A4z�A2A�A1�A.��A,n�A+A*��A'�A%�A#��A#�A"A�A!"�A ĜA  �A��AȴAAp�A;dA%Az�A1A�A�/A�DAVA1'A�mA��A��At�AXA
=A�DA=qA$�A�A��AdZA�`A�AhsA�\AA��A��A=qAƨA��A�wA��A�;A�A�A�A��A��AQ�A�A;dA
=A�HA�9A��A��A��A��A
�A
-A	7LA��AoA�A��A�A �jA -@�
=@�O�@��@�bN@�(�@��m@�K�@��@���@�`B@��@���@��u@��F@��y@�ff@�=q@�$�@��@�@��#@��-@��-@���@��^@�F@�p�@�b@�\)@��@�/@���@�ƨ@��@��/@�l�@�33@���@�7L@���@�"�@�n�@�J@�J@ᙚ@�%@�@�1'@�+@���@�M�@�V@��;@��@�|�@�v�@�5?@�5?@�J@ݑh@�Z@ۥ�@�@�E�@�M�@��@�x�@�p�@�/@؛�@�Z@�(�@��;@�K�@ָR@�J@�~�@�v�@�G�@��/@�9X@��y@�@щ7@�?}@�`B@�x�@с@�S�@��@�`B@�%@�V@͡�@͙�@�%@̴9@�bN@�b@�+@ʗ�@��@�~�@�E�@�J@��T@���@ɲ-@�hs@�V@�j@�l�@�33@�l�@�;d@�ȴ@�5?@Ų-@ũ�@�O�@ļj@�Z@� �@Õ�@�~�@�@���@��@�Q�@��@�ƨ@�C�@���@��#@���@�j@��@��w@���@�C�@��@��@���@���@�@��@���@��+@�^5@�{@���@�hs@���@�9X@��@�t�@�"�@�
=@��H@���@��@���@�X@���@�z�@�1'@� �@�ƨ@�dZ@��\@�hs@�&�@��`@��@�Q�@�I�@���@�ƨ@���@�;d@�^5@��h@�/@���@���@��@�j@�Q�@�Z@�1'@��@��@�ƨ@�t�@��H@��+@��T@��-@�/@��@�I�@��@���@���@�\)@�;d@�"�@�o@��y@���@��#@���@�x�@�hs@�`B@�7L@���@��9@��@�Z@��w@��P@�o@��+@�-@��@���@���@�p�@�/@�V@���@��@��`@���@�bN@� �@�ƨ@�33@��@�V@��@��`@���@�bN@�I�@�A�@�9X@��@��@��w@�l�@�+@���@�~�@�5?@��@�/@��9@�r�@�A�@�ƨ@�|�@�dZ@�\)@��@��!@��+@�E�@�J@�p�@�/@�&�@�%@���@���@�o@��7@���@{��@r~�@fV@[�F@TI�@MV@D�j@=p�@4(�@-�@%�@K�@��@\)@9X@X@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	t�B	s�B	s�B	s�B	t�B	s�B	t�B	t�B	s�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	r�B	q�B	n�B	m�B	o�B	p�B	s�B	v�B	z�B	� B	�JB	�hB	��B	�!B	�-B	��B	�TB	��B
B
B
PB
�B	�yB	�mB	��B	�RB	�'B	�B	�'B	�!B	�!B	�B	�RB	�dB	ĜB	��B	�ZB
PB
�B
:^B
C�B
q�B
��BhBC�Bt�B�1B��B�3BĜB+B�B-B)�B$�B(�B:^B33B.B%�BuB�sB��B�qB�7BZB/B�B
��B
��B
�LB
��B
�PB
o�B
F�B
hB	�B	�5B	ŢB	��B	v�B	`BB	N�B	E�B	@�B	;dB	33B	)�B	�B	bB	B�B�ZB�)B�B�
B�B��B��B��B��B��B��B��B�B��B��B��B��B��B�
B�
B�5B�;B�B�B��B��B�#B�NB�ZB�ZB�fB�B�B��B��B	+B	oB	�B	�B	!�B	&�B	+B	7LB	;dB	E�B	M�B	P�B	ZB	_;B	e`B	p�B	t�B	~�B	�=B	�\B	�\B	�bB	�bB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�^B	��B	ŢB	ŢB	ŢB	ŢB	ƨB	ŢB	B	B	ÖB	B	��B	��B	��B	��B	�}B	�jB	�XB	�'B	��B	��B	�VB	�B	z�B	r�B	m�B	hsB	bNB	_;B	_;B	^5B	^5B	_;B	aHB	aHB	ffB	gmB	iyB	l�B	o�B	s�B	x�B	�B	�1B	�7B	�=B	�JB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�9B	�3B	�!B	�B	�B	�B	�dB	�qB	�jB	�dB	�dB	�dB	�jB	�qB	�jB	�jB	�dB	�jB	�jB	�qB	�jB	�jB	�dB	�jB	�qB	�wB	�}B	�qB	�dB	�^B	ÖB	ŢB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�
B	�B	��B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�BB	�BB	�BB	�NB	�`B	�`B	�`B	�`B	�`B	�`B	�mB	�B	�B	�B	�yB	�yB	�yB	�sB	�yB	�B	�B	�yB	�yB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B	��B
  B
B
  B
  B
B
B
B
  B
  B
B
B
B
B
B
B
B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
+B
+B
+B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
DB

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB

=B
DB
DB
DB
JB
PB
PB
VB
\B
\B
\B
\B
bB
bB
bB
bB
\B
\B
\B
VB
bB
oB
hB
bB
\B
\B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
\B
\B
\B
\B
hB
oB
�B
(�B
'�B
6FB
;dB
A�B
D�B
I�B
N�B
S�B
XB
_;B
dZB
jB
m�B
r�B
v�B
x�B
z�B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	t�B	s�B	s�B	s�B	t�B	s�B	t�B	t�B	s�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	t�B	r�B	q�B	nmB	mhB	osB	p|B	s�B	v�B	z�B	�B	�#B	�DB	��B	��B	�B	ϵB	�,B	��B
�B
�B
(B
�B	�PB	�EB	ͨB	�&B	��B	��B	� B	��B	��B	��B	�(B	�8B	�pB	��B	�1B
'B
�B
:2B
CkB
q}B
�bB=BChBt�B�B��B�B�qB�B�B,�B)�B$�B(�B:1B3B-�B%�BCB�CBѻB�BB�BY�B.�BtB
��B
��B
�B
��B
�B
omB
FrB
3B	�zB	�B	�mB	�tB	v�B	`B	N�B	EiB	@LB	;-B	2�B	)�B	yB	+B	�B�{B�B��B��B��B��B��BӾBҸBѰBϥB��B��B��B��B��BѵBѱBҷB��B��B��B��B��B��BӹBӸB��B�B�B�B�'B�SB�kB��B��B	�B	.B	jB	zB	!�B	&�B	*�B	7B	;'B	EeB	M�B	P�B	Y�B	^�B	e!B	peB	t|B	~�B	� B	�B	�B	�"B	�$B	�*B	�6B	�>B	�HB	�GB	�AB	�KB	�hB	�fB	�hB	�rB	��B	�B	�IB	�dB	�cB	�cB	�aB	�hB	�bB	�NB	�NB	�VB	�OB	�JB	�CB	�MB	�IB	�<B	�+B	�B	��B	��B	�iB	�B	��B	z�B	rlB	mPB	h0B	bB	^�B	^�B	]�B	]�B	^�B	aB	aB	f&B	g*B	i6B	lIB	o\B	ssB	x�B	��B	��B	��B	��B	�B	�B	� B	�6B	�EB	�vB	��B	�xB	�vB	�~B	�tB	�oB	�_B	�EB	�DB	�IB	�iB	�wB	�vB	�uB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�!B	�.B	�'B	�"B	�!B	�!B	�&B	�/B	�(B	�&B	� B	�(B	�'B	�,B	�)B	�&B	�!B	�'B	�/B	�5B	�9B	�.B	�"B	�B	�TB	�_B	�iB	�vB	�yB	�wB	˃B	�~B	˃B	үB	��B	��B	ӵB	ϟB	ΖB	ϛB	ѫB	��B	��B	��B	��B	��B	��B	տB	ԼB	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�)B	�:B	�;B	�:B	�6B	�5B	�5B	�/B	�4B	�:B	�<B	�5B	�4B	�-B	�2B	�6B	�6B	�7B	�IB	�UB	�LB	�UB	�UB	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
 �B	��B	��B
 �B	��B	��B
 �B
 �B
 �B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
	�B

�B

�B
 B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
 B

�B

�B
 B
	�B
B

�B

�B
B
B

B
B
B
B
B
B
 B
B
B
B
B
B
B
B
B
)B
#B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
)B
ZB
(�B
'�B
6 B
; B
ADB
DVB
IwB
N�B
S�B
W�B
^�B
dB
j9B
mLB
rkB
v�B
x�B
z�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.53 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007542019040510075420190405100754  AO  ARCAADJP                                                                    20181121125946    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125946  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125946  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100754  IP                  G�O�G�O�G�O�                