CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:58Z creation      
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
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125958  20190405100802  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @����@�/1   @���8�0@-��hr��c�7Kƨ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDts3Dy��D��D�I�D�� D���D��3D�L�D���D��fD���D�,�D���D�� D���D�9�Dڃ3D�3D�	�D�S3D�3D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @`��@�Q�@�Q�A(�A((�AH(�Ah(�A�{A�{A�{A�{A��HA�{A�{A�{B
=B

=B
=B
=B"
=B*
=B2
=B:
=BB
=BJ
=BR
=BZ
=Bb
=Bj
=Br
=By��B�B�B�8RB�B�B�B�B�B�B�B�B�B�B�B�8RB�8RB�B�B�B�B�B���B�B�B�B���B�B�B�B�B�B�C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(�)C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX�)CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|�)C~��C�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�NC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHC�AHD  �D ��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D	 �D	��D
 �D
��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D �D��D  �D ��D! �D!��D" �D"��D# �D#��D$ �D$��D% �D%��D& �D&��D' �D'��D( �D(��D) �D)��D* �D*��D+ �D+��D, �D,��D- �D-��D. �D.��D/ �D/��D0 �D0��D1 �D1��D2 �D2��D3 �D3��D4 �D4��D5 �D5��D6 �D6��D7 �D7��D8 �D8��D9 �D9��D: �D:��D; �D;��D< �D<��D= �D=��D> �D>��D? �D?��D@ �D@��DA �DA��DB �DB��DC �DC��DD �DD��DE �DE��DF �DF��DG �DG��DH �DH��DI �DI��DJ �DJ��DK �DK��DL �DL��DM �DM��DN �DN��DO �DO��DP �DP��DQ �DQ��DR �DR��DS �DS��DT �DT��DU �DU��DV �DV��DW �DW��DX �DX��DY �DY��DZ �DZ��D[ �D[��D\ �D\��D] �D]��D^ �D^��D_ �D_��D` �D`��Da �Da��Db �Db��Dc �Dc��Dd �Dd��De �De��Df �Df��Dg �Dg��Dh �Dh��Di �Di��Dj �Dj��Dk �Dk��Dl �Dl��Dm �Dm��Dn �Dn��Do �Do��Dp �Dp��Dq �Dq��Dr �Dr��Ds �Ds��Dt'
Dt��Dy�qD�-D�Y�D��RD��D��D�]D��D��D�D�=D���D��RD�	�D�I�Dړ�D�ÅD��D�c�D�D��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�S�A�VA�S�A�S�A�VA�VA�VA�XA�XA�XA�ZA�ZA�ZA�ZA�\)A�\)A�^5A�\)A�ZA�\)A�`BA�`BA�\)A�\)A�\)A�^5A�`BA�^5A�S�A�/A��A�dZAɩ�AɋDA�=qA��`A�XA���A�~�A��A�7LA�\)A�v�Aĺ^A���A�S�Aå�Aß�AÉ7Aº^A��A�oA�VA���A�hsA�S�A�M�A�C�A��\A�O�A�`BA�x�A���A��TA� �A�;dA���A�C�A��^A���A��`A�JA��\A���A���A�I�A�ƨA���A��A��A��A� �A�JA�t�A�x�A�A�bNA�&�A�v�A��A�9XA�  A��wA��A�O�A���A���A���A}7LA{|�AzZAx�RAu�#Au+AtVAp��Ak;dAeK�AcoA`�A^�A\��A[/AV�uAT�DAS�AQ�AP�yAPE�ANZAK��AG��AFJACt�AA��A@�+A>��A<�A<{A;"�A97LA6��A5�A4M�A3�A2��A2(�A1dZA0(�A/�FA.�jA-"�A-A,��A,��A*^5A)hsA)�A)\)A)+A(��A(�A'K�A&~�A$��A!�A!�7A!�mA"�9A#33A"z�A �yA�AA�A�#A�yAjAA��AA7LA��A��AJA1AA�DA9XA�yA�!A�A�;A�!A&�A�AXA+AjA{A|�A
��A
M�A	��A	\)A	&�A�/Ar�A1A�TA��A?}A��AjAAS�A��Ar�A�mA|�AC�A��AjA{AO�AVA �+A b@�o@�ff@��9@���@�~�@�7L@�`B@�ƨ@��+@��!@�V@��@�O�@�dZ@��@���@��/@�bN@땁@�@�l�@���@�+@�
=@�dZ@��`@�|�@�C�@�V@噚@�9@�
=@��@�x�@�-@��@�Q�@��@�ff@�^5@�7L@�9X@�\)@�C�@���@�^5@���@�1'@��
@ו�@�@և+@�{@��#@թ�@��#@�@��T@�hs@�Ĝ@�Z@�Q�@��;@Ӆ@�;d@��y@ҧ�@�=q@�J@�=q@�O�@Л�@��@�ƨ@�|�@���@��@��`@�Q�@˕�@���@�dZ@�C�@��H@�ff@���@�O�@�?}@�I�@�v�@�n�@ǅ@��;@�  @��H@ź^@�x�@�M�@ƸR@Ƈ+@�$�@��@��@�-@��#@���@ċD@�Q�@�Z@�(�@�@���@��/@��@��u@�j@���@��@�+@�-@��@���@���@��@�`B@�V@�Ĝ@�1'@�S�@�+@�^5@��@��@�7L@���@���@���@�&�@�7L@��@���@��D@�I�@��m@�\)@��y@�n�@�@�/@���@��u@�Z@��@��u@��
@�
=@���@��;@�"�@���@�E�@��@�`B@�?}@��u@��@�ƨ@�t�@�o@��@���@�ff@�J@��@�%@��/@��u@��;@�|�@��H@���@�=q@���@���@���@��@��@�bN@��@��
@���@�dZ@���@���@���@�=q@�{@���@���@�x�@��@��9@�Z@��m@�|�@�K�@�@���@���@�p�@�%@���@�Ĝ@��@��@��@��D@��@� �@���@��
@�9X@�I�@��m@�33@���@�=q@��@�j@��m@���@�\)@��@�ȴ@�~�@�-@�{@���@��@���@��j@�z�@�A�@� �@��@��F@�\)@�@��@��R@��!@�-@�{@�=q@��h@��@���@��j@�9X@��@��m@�t�@��R@���@�E�@�5?@��@�J@��@���@�`B@��@�G�@��!@���@z��@st�@k��@a�#@U�T@K�
@B�H@<(�@6V@/|�@)��@%`B@�@�@@bN@1@b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�S�A�VA�S�A�S�A�VA�VA�VA�XA�XA�XA�ZA�ZA�ZA�ZA�\)A�\)A�^5A�\)A�ZA�\)A�`BA�`BA�\)A�\)A�\)A�^5A�`BA�^5A�S�A�/A��A�dZAɩ�AɋDA�=qA��`A�XA���A�~�A��A�7LA�\)A�v�Aĺ^A���A�S�Aå�Aß�AÉ7Aº^A��A�oA�VA���A�hsA�S�A�M�A�C�A��\A�O�A�`BA�x�A���A��TA� �A�;dA���A�C�A��^A���A��`A�JA��\A���A���A�I�A�ƨA���A��A��A��A� �A�JA�t�A�x�A�A�bNA�&�A�v�A��A�9XA�  A��wA��A�O�A���A���A���A}7LA{|�AzZAx�RAu�#Au+AtVAp��Ak;dAeK�AcoA`�A^�A\��A[/AV�uAT�DAS�AQ�AP�yAPE�ANZAK��AG��AFJACt�AA��A@�+A>��A<�A<{A;"�A97LA6��A5�A4M�A3�A2��A2(�A1dZA0(�A/�FA.�jA-"�A-A,��A,��A*^5A)hsA)�A)\)A)+A(��A(�A'K�A&~�A$��A!�A!�7A!�mA"�9A#33A"z�A �yA�AA�A�#A�yAjAA��AA7LA��A��AJA1AA�DA9XA�yA�!A�A�;A�!A&�A�AXA+AjA{A|�A
��A
M�A	��A	\)A	&�A�/Ar�A1A�TA��A?}A��AjAAS�A��Ar�A�mA|�AC�A��AjA{AO�AVA �+A b@�o@�ff@��9@���@�~�@�7L@�`B@�ƨ@��+@��!@�V@��@�O�@�dZ@��@���@��/@�bN@땁@�@�l�@���@�+@�
=@�dZ@��`@�|�@�C�@�V@噚@�9@�
=@��@�x�@�-@��@�Q�@��@�ff@�^5@�7L@�9X@�\)@�C�@���@�^5@���@�1'@��
@ו�@�@և+@�{@��#@թ�@��#@�@��T@�hs@�Ĝ@�Z@�Q�@��;@Ӆ@�;d@��y@ҧ�@�=q@�J@�=q@�O�@Л�@��@�ƨ@�|�@���@��@��`@�Q�@˕�@���@�dZ@�C�@��H@�ff@���@�O�@�?}@�I�@�v�@�n�@ǅ@��;@�  @��H@ź^@�x�@�M�@ƸR@Ƈ+@�$�@��@��@�-@��#@���@ċD@�Q�@�Z@�(�@�@���@��/@��@��u@�j@���@��@�+@�-@��@���@���@��@�`B@�V@�Ĝ@�1'@�S�@�+@�^5@��@��@�7L@���@���@���@�&�@�7L@��@���@��D@�I�@��m@�\)@��y@�n�@�@�/@���@��u@�Z@��@��u@��
@�
=@���@��;@�"�@���@�E�@��@�`B@�?}@��u@��@�ƨ@�t�@�o@��@���@�ff@�J@��@�%@��/@��u@��;@�|�@��H@���@�=q@���@���@���@��@��@�bN@��@��
@���@�dZ@���@���@���@�=q@�{@���@���@�x�@��@��9@�Z@��m@�|�@�K�@�@���@���@�p�@�%@���@�Ĝ@��@��@��@��D@��@� �@���@��
@�9X@�I�@��m@�33@���@�=q@��@�j@��m@���@�\)@��@�ȴ@�~�@�-@�{@���@��@���@��j@�z�@�A�@� �@��@��F@�\)@�@��@��R@��!@�-@�{@�=q@��h@��@���@��j@�9X@��@��m@�t�@��R@���@�E�@�5?@��@�J@��@���@�`B@��@�G�@��!@���@z��@st�@k��@a�#@U�T@K�
@B�H@<(�@6V@/|�@)��@%`B@�@�@@bN@1@b111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B�{B��B��B��B��B�{B�{B�{B��B��B��B��B��B��B�{B��B��B��B��B�{B�{B�{B�{B��B��B��B��B��B�-B	  B	�B	ĜB	�B	��B
�B
33B
J�B
O�B
L�B
dZB
�B!�B �B2-B8RBF�BXBq�B}�B��B�HB�B	7BhBbBVB �B0!B33B8RB<jB@�BB�BE�BD�BC�B?}B:^B/B�B�;B�/B�BB�LB��B��B�uB�\Bz�B�=B�uB�DBz�B}�BbNBD�B,BB
�B
�)B
�qB
�PB
r�B
Q�B
<jB
1'B
�B
VB
B	��B	�fB	�)B	��B	�FB	�{B	n�B	YB	A�B	2-B	(�B	�B	\B	1B	%B	B	B	B��B�B�/B��B��B�#B�HB�B�#B�fB��B��B��B	+B	bB	!�B	#�B	%�B	%�B	&�B	%�B	&�B	"�B	$�B	%�B	'�B	,B	49B	>wB	L�B	[#B	`BB	ffB	dZB	e`B	aHB	XB	YB	k�B	�DB	��B	��B	�uB	�VB	�VB	�VB	�DB	�PB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	�=B	hsB	ZB	VB	]/B	u�B	�B	�B	�PB	�bB	�bB	�VB	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�B	�B	�!B	�!B	�'B	�LB	�?B	�9B	�9B	�9B	�FB	�FB	�3B	�-B	�!B	�B	�FB	�9B	�3B	�LB	�3B	�B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�3B	�LB	�jB	��B	�RB	�RB	�dB	�wB	�qB	�jB	�XB	�FB	�}B	ǮB	ĜB	��B	�jB	�dB	�jB	�XB	�LB	�RB	�dB	�jB	�jB	�^B	�^B	�^B	�^B	�dB	�jB	�qB	�wB	��B	B	ŢB	ȴB	ȴB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�)B	�;B	�)B	�B	�#B	�`B	�sB	�sB	�`B	�HB	�NB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
%B
B
B
%B
+B
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

=B

=B
DB
DB
JB
JB
JB
JB
JB
JB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
\B
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
bB
\B
VB
PB
JB
JB
PB
PB
VB
VB
VB
VB
VB
VB
\B
oB
�B
{B
{B
uB
oB
bB
hB
hB
oB
oB
oB
oB
oB
uB
{B
{B
{B
{B
{B
{B
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
#�B
#�B
"�B
#�B
$�B
(�B
2-B
8RB
:^B
@�B
D�B
L�B
Q�B
XB
]/B
_;B
dZB
hsB
k�B
q�B
s�B
w�B
{�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�WB�UB�ZB�WB�WB�UB�RB�PB�QB�ZB�ZB�ZB�ZB�UB�ZB�RB�WB�ZB�WB�ZB�TB�TB�PB�RB�WB�kB�kB��B��B�B��B	��B	�uB	��B	��B
dB
3	B
J�B
O�B
L�B
d,B
�UB!�B �B2B8$BF{BW�Bq~B}�B��B�B�sB	B<B5B(B �B/�B3B8&B<AB@UBBaBEuBDmBCjB?NB:/B.�B�B�B�B��B�`B� B��B�SB�HB�,Bz�B�B�GB�Bz�B}�BbBDkB+�B�B
�eB
��B
�AB
�B
r|B
Q�B
<5B
0�B
vB
!B
�B	��B	�/B	��B	ЮB	�B	�EB	ncB	X�B	ASB	1�B	(�B	�B	&B	�B	�B	�B	�B	�B��B�YB��BЮBˑB��B�B��B��B�,B��B��B��B	�B	)B	!�B	#�B	%�B	%�B	&�B	%�B	&�B	"�B	$�B	%�B	'�B	+�B	3�B	>;B	L�B	Z�B	`B	f+B	dB	e$B	aB	W�B	X�B	kKB	�	B	�qB	�fB	�:B	�B	�B	�B	�B	�B	�%B	�2B	�@B	�GB	�ZB	�jB	�nB	�vB	�|B	�oB	�B	h5B	Y�B	U�B	\�B	u�B	��B	��B	�B	�#B	�$B	�B	�IB	�OB	�IB	�<B	�OB	�gB	�zB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�	B	�B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�,B	�CB	�B	�B	�%B	�8B	�0B	�+B	�B	�B	�@B	�mB	�\B	�CB	�+B	�$B	�)B	�B	�B	�B	�$B	�,B	�+B	�B	�B	�B	�B	�#B	�)B	�1B	�6B	�@B	�QB	�cB	�tB	�uB	�oB	�sB	ʃB	̋B	̌B	̌B	͒B	ФB	ФB	ХB	ԾB	ӸB	ҰB	ҰB	ұB	��B	��B	ӷB	ѩB	ФB	ϝB	ϝB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�3B	�0B	�B	�B	�B	�3B	�HB	�^B	�cB	�_B	�dB	�uB	��B	��B	��B	��B	��B	��B	�mB	�FB	�OB	�MB	�\B	�dB	�\B	�dB	��B	��B	�tB	�nB	�jB	�uB	��B	�{B	�sB	�hB	�YB	�VB	�UB	�eB	�\B	�^B	�^B	�cB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
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
B
 B
B
B
	B
	B
	B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
 B
 B
!B
B
'B
&B
&B
"B
 B
B
B
B
B
	B
B
B
B
B
B
B
B
B
B
.B
?B
;B
9B
1B
/B
!B
'B
'B
0B
-B
+B
.B
-B
1B
7B
8B
:B
8B
8B
9B
0B
4B
6B
=B
@B
?B
FB
JB
YB
OB
^B
bB
gB
dB
kB
sB
oB
jB
kB
nB
iB
cB
cB
wB
zB
"�B
#�B
#�B
"�B
#�B
$�B
(�B
1�B
8B
:B
@@B
DXB
L�B
Q�B
W�B
\�B
^�B
dB
h2B
k@B
qhB
ssB
w�B
{�B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.51 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008022019040510080220190405100802  AO  ARCAADJP                                                                    20181121125958    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125958  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125958  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100802  IP                  G�O�G�O�G�O�                