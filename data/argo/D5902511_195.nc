CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  9   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-01-05T06:46:31Z creation; 2022-02-04T23:30:07Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  V�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  ]D   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  w   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  }�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �H   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  �L   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t +�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 2    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` K�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   L(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   R(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   X(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ^(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ^|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ^�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ^�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ^�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ^�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   _   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   _8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        _`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        _h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       _p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _xArgo profile    3.1 1.2 19500101000000  20220105064631  20220204223522  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_195                 6810_008521_195                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ٯ�0��@ٯ�0��11  @ٯ�`A�7@ٯ�`A�7@133333@133333�d{��S���d{��S��11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @@  @}p�@��R@�  @��
A�A  A   A,(�AAG�Aa�A�Q�A�  A�  A���A���AϮA߮A�  B (�BQ�B(�B  B   B(  B0(�B7�B?�
BH  BP  BX  B`  Bh  Bp(�BxQ�B�{B�{B�(�B�(�B�{B�{B�  B�{B�{B�{B�{B�  B��B�  B�(�B�{B�  B�  B�  B�(�B�{B�  B�  B��
B�  B�(�B�  B��
B�  B�(�B�  B�{C {C  C  C�C��C
  C
=C
=C  C  C
=C
=C{C
=C  C  C��C"  C${C&{C({C*
=C,{C.{C0{C2
=C4  C5��C7�C9��C<  C>
=C@
=CB
=CD  CF  CG��CI��CL  CN
=CP
=CR�CT
=CV
=CX
=CY�C\  C^{C_��Cb  Cc��Cf  Cg��Cj  Cl  Cm��Co��Cq��Ct
=Cv
=Cx  Cz  C|  C~  C��C�  C�  C�
=C�C���C���C���C�C���C���C�C���C�  C���C���C���C�C�  C���C�C���C���C���C���C�C�  C�C�  C���C�C�C�
=C�  C�  C�  C�  C�
=C�C���C�  C�  C�C�C���C�  C���C���C�C�  C���C�  C�C�C�C�  C���C���C�  C�  C���C�  C�
=C�C�  C�C�  C�  C�  C�C�  C�  C�  C�  C�  C�  C���C�  C�C�
=C�C�C���C���C���C�  C���C�C�
=C�C�  C���C�C�C�  C�  C���C���C�C�  C�  C�C�  C���C�  C���C�  C�C�  C���C���C���C�C�  C���C���C�  C���C���C�  C�  C�  C�C���C���C�  C�  C�  C���D z�D �qD� D  D� D  D� D�qD}qD  D��D�D}qD�qD}qD�D��D�qD	z�D
  D
��D  D� D��Dz�D�qD��D�D}qD  D� D  D��D�D��D�D� D�D��D�D��DD��DD� D�qD��D  D� D  D��D�qD}qDD� D��D� DD�DD�DD��D�qD � D �qD!xRD!��D"��D#�D#��D$D$}qD%�D%��D%�qD&� D'�D'� D'�qD(}qD)�D)� D)��D*� D+D+��D,�D,}qD,��D-z�D-�RD.z�D/�D/�D0  D0�D1�D1�D2�D2}qD3  D3��D3�qD4z�D4�RD5� D6D6��D7�D7� D7��D8}qD9  D9}qD9��D:}qD;  D;z�D;�RD<z�D=  D=� D=�qD>}qD>�qD?� D@  D@}qDA  DA� DA�qDB}qDC�DC��DD�DD}qDE  DE�DFDF� DG�DG��DH�DH��DH�qDI}qDJ�DJ� DJ�qDK}qDK�qDL� DM�DM��DN  DN}qDN�qDO� DP�DP�DQDQ� DR  DR��DS�DS}qDT  DT��DT�qDU}qDV�DV��DW  DW� DX  DX��DY�DY}qDZ�DZ��D[�D[�D\�D\��D]  D]� D]�qD^}qD^�qD_��D`D`� D`�qDa� Db  Db� Dc  Dc}qDd  Dd� De  De� Df  Df��Dg  Dg� Dh  Dh� Dh�qDi� Di�qDj}qDk�Dk}qDk��Dl}qDm  Dm��Dn�Dn}qDn�qDo��Dp�Dp��Dq  Dq}qDr  Dr��Ds�Ds��Dt  Dt� Dt�qDu}qDv  Dv� Dv�qDw� Dx  Dx}qDx�qDy}qDy�qDz� Dz�qD{� D|  D|}qD|�qD}z�D}�qD~��DD�D�HD�@ D�� D�� D���D�@ D�� D��HD���D�>�D�~�D�� D��D�@ D�� D��HD�  D�AHD�� D��HD�  D�@ D�~�D��qD���D�@ D��HD�D�HD�@ D�}qD��qD�  D�AHD�� D���D��qD�>�D�� D�D�HD�@ D�� D��HD�  D�@ D�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>���>�G�?L��?�  ?�33?�(�@   @�R@0��@B�\@^�R@s33@��
@��@��H@��@�33@�p�@Ǯ@�@�\@�@�
=A�A
=A(�A�
AQ�A�RA$z�A(��A/\)A5A9��A@  AFffAK�AP  AW
=A\��AaG�Ag
=Amp�Aq�AxQ�A~�RA���A�(�A��A�=qA���A�Q�A��\A�p�A���A��\A�A�G�A�33A�A�G�A��
A�{A���A�z�A�ffA�G�A�z�A�\)A�G�A�z�A�  A��A���A�  A�=qA��A�Q�Aڏ\A�p�A��A�33A�A��A��
A�A��A��
A�{A���A�(�A�{B Q�B�B33BQ�B{B\)BQ�B
{B33Bz�B=qB33B��B=qB�B��BffB�
B��B�\B�
B��B�\B Q�B!G�B"�RB$Q�B%��B&�RB(z�B)�B+
=B,��B.{B/33B0��B2ffB3�B4��B6�\B7�B8��B:ffB<  B=�B>=qB?�
BAG�BBffBC�
BEp�BF�\BG�
BI��BJ�\BL(�BMBO33BP(�BQBS\)BTz�BUBW�BX��BZ{B[�
B]�B^{B_�BaG�Bb�\Bc�Be�Bf�HBh  Bi�Bj�HBlQ�Bm��Bn�HBp��Bq�Bs33Bt��BvffBw�Bx��Bz�\B|  B|��B~�RB�{B��\B�33B�  B��\B�
=B���B�Q�B��HB�33B��B�ffB���B�p�B�  B�=qB��RB�G�B��B��B�z�B���B�33B��B�(�B��\B��HB�p�B��
B�{B��\B��B�\)B��
B�ffB���B�
=B���B��
B�ffB���B�
=B��B��B�ffB��HB�
=B��B�  B�ffB���B�\)B���B�{B���B�
=B�\)B��B�Q�B���B�
=B��B�(�B�z�B���B�p�B��
B�(�B��RB�G�B���B�  B��\B�
=B�\)B��
B�z�B���B��B�B�(�B��\B��B���B��
B��\B��HB�G�B��B�Q�B��RB�\)B��B�{B��RB�
=B��B�(�B�ffB��HB��B��B�=qB��HB�G�B��B�Q�B���B��B��B�Q�B���B�
=B��B�{B�z�B��B���B��B�ffB�
=B��B��
B�=qB��HB�p�B�B�(�B��RB�G�B���B��B���B���B�\)B�  B�ffB���B�G�B�B�{B�z�B��B��B��
B�=qB��HB�\)BîB�{Bģ�B��B�p�B��
B�ffB��HB�33BǮB�=qBȏ\B���B�p�B�  B�ffBʸRB�G�B�B�{B�z�B�
=BͅB��
B�=qBθRB�33BυB��
B�ffB���B�33Bљ�B�{B�z�B���B�33B��
B�(�B�ffB���B�p�B�B�{B֏\B��B�p�B�B�{Bأ�B��B�\)B�B�=qBڸRB�
=B�\)B��
B�ffBܣ�B���B�p�B�  B�Q�Bޣ�B�
=Bߙ�B��B�=qB�RB�33B�B�B�=qB�RB�
=B�G�B�B�=qB�\B���B�p�B�B�  B�\B�
=B�G�B�B��B�z�B���B�
=B陚B�  B�Q�B��B�
=B뙚B�  B�ffB�RB�
=B홚B�{B�Q�B��B��B�B�  B�=qB�RB�G�B�B�B�=qB���B�
=B�G�B�B�(�B�\B���B��B�\)B��
B�=qB��\B���B�
=B�\)B��B�Q�B���B���B�G�B�B�(�B�Q�B���B�G�B��B��B�=qB���B�33B��B�B�(�B��RB�33B��B��
C {C =qC z�C �RC �HC
=C(�CffC��C�
C��C�CQ�C��CC�HC{C\)Cz�C��C�HC{C33CffC��C�
C  C�CG�C�C�RC��C��C33CffC�C��C�C�C33C\)C��C��C�HC{CQ�Cp�C�\C��C	  C	�C	=qC	z�C	��C	C	��C
33C
\)C
p�C
�RC
�C  C33Cz�C��CC��C33CQ�Cz�C�RC�C  C33Cp�C�\C�RC�C33CQ�Cz�C�RC�C
=C33Cz�C��C�RC  C33CG�C�CC�
C
=CG�Cp�C�C�RC��C{C33Cp�C�CC��C(�CffC�\C�C�HC�C=qCffC�C�
C��C33C\)C�CC  C{CG�C�C��C�C{CG�C�\CC�C{C\)C�\C��C�C{CffC��CC��C=qCp�C��C��C�CG�Cp�C��C��C�C=qC�C��C��C{C\)C��C�
C
=C33Cz�CC�C�CQ�C��C�
C��C 33C z�C �RC �C!{C!Q�C!��C!�
C!��C"=qC"�C"�RC"�C#{C#ffC#��C#C$  C$G�C$�\C$�RC$�HC%�C%p�C%��C%�
C%��C&33C&z�C&�RC&�HC'{C'\)C'��C'��C'�C(33C(�C(�RC(�
C)�C)ffC)��C)C*{C*Q�C*�C*�RC+  C+G�C+�C+�C+�C,33C,�C,�C,�HC-(�C-p�C-��C-��C.
=C.Q�C.��C.�RC.��C/=qC/z�C/�RC/�HC0{C0\)C0��C0�
C1  C133C1z�C1�RC1��C2�C2Q�C2�\C2�
C3{C3=qC3p�C3�RC3��C433C4\)C4�\C4�HC5{C5=qC5z�C5C6
=C633C6\)C6��C6��C7(�C7Q�C7�\C7��C8{C8G�C8p�C8C9
=C9=qC9ffC9�RC:  C:=qC:ffC:��C:�C;33C;p�C;��C;�
C<{C<ffC<�C<�
C=
=C=Q�C=��C=�HC>�C>Q�C>�C>��C?{C?\)C?��C?�HC@
=C@G�C@�\C@�
CA{CAG�CAz�CACB
=CBG�CB�\CBCB�CC(�CCp�CC�CD  CD33CDffCD��CD�CE33CEp�CE�RCE��CF(�CFffCF��CF��CG=qCGz�CG�RCH  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                     111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ?�  @   @@  @}p�@��R@�  @��
A�A  A   A,(�AAG�Aa�A�Q�A�  A�  A���A���AϮA߮A�  B (�BQ�B(�B  B   B(  B0(�B7�B?�
BH  BP  BX  B`  Bh  Bp(�BxQ�B�{B�{B�(�B�(�B�{B�{B�  B�{B�{B�{B�{B�  B��B�  B�(�B�{B�  B�  B�  B�(�B�{B�  B�  B��
B�  B�(�B�  B��
B�  B�(�B�  B�{C {C  C  C�C��C
  C
=C
=C  C  C
=C
=C{C
=C  C  C��C"  C${C&{C({C*
=C,{C.{C0{C2
=C4  C5��C7�C9��C<  C>
=C@
=CB
=CD  CF  CG��CI��CL  CN
=CP
=CR�CT
=CV
=CX
=CY�C\  C^{C_��Cb  Cc��Cf  Cg��Cj  Cl  Cm��Co��Cq��Ct
=Cv
=Cx  Cz  C|  C~  C��C�  C�  C�
=C�C���C���C���C�C���C���C�C���C�  C���C���C���C�C�  C���C�C���C���C���C���C�C�  C�C�  C���C�C�C�
=C�  C�  C�  C�  C�
=C�C���C�  C�  C�C�C���C�  C���C���C�C�  C���C�  C�C�C�C�  C���C���C�  C�  C���C�  C�
=C�C�  C�C�  C�  C�  C�C�  C�  C�  C�  C�  C�  C���C�  C�C�
=C�C�C���C���C���C�  C���C�C�
=C�C�  C���C�C�C�  C�  C���C���C�C�  C�  C�C�  C���C�  C���C�  C�C�  C���C���C���C�C�  C���C���C�  C���C���C�  C�  C�  C�C���C���C�  C�  C�  C���D z�D �qD� D  D� D  D� D�qD}qD  D��D�D}qD�qD}qD�D��D�qD	z�D
  D
��D  D� D��Dz�D�qD��D�D}qD  D� D  D��D�D��D�D� D�D��D�D��DD��DD� D�qD��D  D� D  D��D�qD}qDD� D��D� DD�DD�DD��D�qD � D �qD!xRD!��D"��D#�D#��D$D$}qD%�D%��D%�qD&� D'�D'� D'�qD(}qD)�D)� D)��D*� D+D+��D,�D,}qD,��D-z�D-�RD.z�D/�D/�D0  D0�D1�D1�D2�D2}qD3  D3��D3�qD4z�D4�RD5� D6D6��D7�D7� D7��D8}qD9  D9}qD9��D:}qD;  D;z�D;�RD<z�D=  D=� D=�qD>}qD>�qD?� D@  D@}qDA  DA� DA�qDB}qDC�DC��DD�DD}qDE  DE�DFDF� DG�DG��DH�DH��DH�qDI}qDJ�DJ� DJ�qDK}qDK�qDL� DM�DM��DN  DN}qDN�qDO� DP�DP�DQDQ� DR  DR��DS�DS}qDT  DT��DT�qDU}qDV�DV��DW  DW� DX  DX��DY�DY}qDZ�DZ��D[�D[�D\�D\��D]  D]� D]�qD^}qD^�qD_��D`D`� D`�qDa� Db  Db� Dc  Dc}qDd  Dd� De  De� Df  Df��Dg  Dg� Dh  Dh� Dh�qDi� Di�qDj}qDk�Dk}qDk��Dl}qDm  Dm��Dn�Dn}qDn�qDo��Dp�Dp��Dq  Dq}qDr  Dr��Ds�Ds��Dt  Dt� Dt�qDu}qDv  Dv� Dv�qDw� Dx  Dx}qDx�qDy}qDy�qDz� Dz�qD{� D|  D|}qD|�qD}z�D}�qD~��DD�D�HD�@ D�� D�� D���D�@ D�� D��HD���D�>�D�~�D�� D��D�@ D�� D��HD�  D�AHD�� D��HD�  D�@ D�~�D��qD���D�@ D��HD�D�HD�@ D�}qD��qD�  D�AHD�� D���D��qD�>�D�� D�D�HD�@ D�� D��HD�  D�@ G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>���>�G�?L��?�  ?�33?�(�@   @�R@0��@B�\@^�R@s33@��
@��@��H@��@�33@�p�@Ǯ@�@�\@�@�
=A�A
=A(�A�
AQ�A�RA$z�A(��A/\)A5A9��A@  AFffAK�AP  AW
=A\��AaG�Ag
=Amp�Aq�AxQ�A~�RA���A�(�A��A�=qA���A�Q�A��\A�p�A���A��\A�A�G�A�33A�A�G�A��
A�{A���A�z�A�ffA�G�A�z�A�\)A�G�A�z�A�  A��A���A�  A�=qA��A�Q�Aڏ\A�p�A��A�33A�A��A��
A�A��A��
A�{A���A�(�A�{B Q�B�B33BQ�B{B\)BQ�B
{B33Bz�B=qB33B��B=qB�B��BffB�
B��B�\B�
B��B�\B Q�B!G�B"�RB$Q�B%��B&�RB(z�B)�B+
=B,��B.{B/33B0��B2ffB3�B4��B6�\B7�B8��B:ffB<  B=�B>=qB?�
BAG�BBffBC�
BEp�BF�\BG�
BI��BJ�\BL(�BMBO33BP(�BQBS\)BTz�BUBW�BX��BZ{B[�
B]�B^{B_�BaG�Bb�\Bc�Be�Bf�HBh  Bi�Bj�HBlQ�Bm��Bn�HBp��Bq�Bs33Bt��BvffBw�Bx��Bz�\B|  B|��B~�RB�{B��\B�33B�  B��\B�
=B���B�Q�B��HB�33B��B�ffB���B�p�B�  B�=qB��RB�G�B��B��B�z�B���B�33B��B�(�B��\B��HB�p�B��
B�{B��\B��B�\)B��
B�ffB���B�
=B���B��
B�ffB���B�
=B��B��B�ffB��HB�
=B��B�  B�ffB���B�\)B���B�{B���B�
=B�\)B��B�Q�B���B�
=B��B�(�B�z�B���B�p�B��
B�(�B��RB�G�B���B�  B��\B�
=B�\)B��
B�z�B���B��B�B�(�B��\B��B���B��
B��\B��HB�G�B��B�Q�B��RB�\)B��B�{B��RB�
=B��B�(�B�ffB��HB��B��B�=qB��HB�G�B��B�Q�B���B��B��B�Q�B���B�
=B��B�{B�z�B��B���B��B�ffB�
=B��B��
B�=qB��HB�p�B�B�(�B��RB�G�B���B��B���B���B�\)B�  B�ffB���B�G�B�B�{B�z�B��B��B��
B�=qB��HB�\)BîB�{Bģ�B��B�p�B��
B�ffB��HB�33BǮB�=qBȏ\B���B�p�B�  B�ffBʸRB�G�B�B�{B�z�B�
=BͅB��
B�=qBθRB�33BυB��
B�ffB���B�33Bљ�B�{B�z�B���B�33B��
B�(�B�ffB���B�p�B�B�{B֏\B��B�p�B�B�{Bأ�B��B�\)B�B�=qBڸRB�
=B�\)B��
B�ffBܣ�B���B�p�B�  B�Q�Bޣ�B�
=Bߙ�B��B�=qB�RB�33B�B�B�=qB�RB�
=B�G�B�B�=qB�\B���B�p�B�B�  B�\B�
=B�G�B�B��B�z�B���B�
=B陚B�  B�Q�B��B�
=B뙚B�  B�ffB�RB�
=B홚B�{B�Q�B��B��B�B�  B�=qB�RB�G�B�B�B�=qB���B�
=B�G�B�B�(�B�\B���B��B�\)B��
B�=qB��\B���B�
=B�\)B��B�Q�B���B���B�G�B�B�(�B�Q�B���B�G�B��B��B�=qB���B�33B��B�B�(�B��RB�33B��B��
C {C =qC z�C �RC �HC
=C(�CffC��C�
C��C�CQ�C��CC�HC{C\)Cz�C��C�HC{C33CffC��C�
C  C�CG�C�C�RC��C��C33CffC�C��C�C�C33C\)C��C��C�HC{CQ�Cp�C�\C��C	  C	�C	=qC	z�C	��C	C	��C
33C
\)C
p�C
�RC
�C  C33Cz�C��CC��C33CQ�Cz�C�RC�C  C33Cp�C�\C�RC�C33CQ�Cz�C�RC�C
=C33Cz�C��C�RC  C33CG�C�CC�
C
=CG�Cp�C�C�RC��C{C33Cp�C�CC��C(�CffC�\C�C�HC�C=qCffC�C�
C��C33C\)C�CC  C{CG�C�C��C�C{CG�C�\CC�C{C\)C�\C��C�C{CffC��CC��C=qCp�C��C��C�CG�Cp�C��C��C�C=qC�C��C��C{C\)C��C�
C
=C33Cz�CC�C�CQ�C��C�
C��C 33C z�C �RC �C!{C!Q�C!��C!�
C!��C"=qC"�C"�RC"�C#{C#ffC#��C#C$  C$G�C$�\C$�RC$�HC%�C%p�C%��C%�
C%��C&33C&z�C&�RC&�HC'{C'\)C'��C'��C'�C(33C(�C(�RC(�
C)�C)ffC)��C)C*{C*Q�C*�C*�RC+  C+G�C+�C+�C+�C,33C,�C,�C,�HC-(�C-p�C-��C-��C.
=C.Q�C.��C.�RC.��C/=qC/z�C/�RC/�HC0{C0\)C0��C0�
C1  C133C1z�C1�RC1��C2�C2Q�C2�\C2�
C3{C3=qC3p�C3�RC3��C433C4\)C4�\C4�HC5{C5=qC5z�C5C6
=C633C6\)C6��C6��C7(�C7Q�C7�\C7��C8{C8G�C8p�C8C9
=C9=qC9ffC9�RC:  C:=qC:ffC:��C:�C;33C;p�C;��C;�
C<{C<ffC<�C<�
C=
=C=Q�C=��C=�HC>�C>Q�C>�C>��C?{C?\)C?��C?�HC@
=C@G�C@�\C@�
CA{CAG�CAz�CACB
=CBG�CB�\CBCB�CC(�CCp�CC�CD  CD33CDffCD��CD�CE33CEp�CE�RCE��CF(�CFffCF��CF��CG=qCGz�CG�RCH  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                     111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�Aө�A�~�A���A�z�A�-A���A��A��yA��yA��yA��mA��TA���Aϴ9AϮAϮAϥ�Aϙ�AϑhAύPAω7AρA�x�A�x�A�x�A�l�A�dZA�ffA�hsA�hsA�hsA�ffA�ZA�K�A�A�A�G�A�O�A�ZA�ffA�v�A�z�A�z�A�~�AυAϏ\Aϟ�AϮA϶FA�A�ȴA���A���A���A���AϼjAϺ^AϺ^Aϴ9AϑhA�;dA�bNA͉7A�G�A̼jA��/A�ZA�hsA�x�A��mA�"�A���A�n�A��A�JA�ZA�7LA�{A�5?A�+A�O�A�33A�VA���A��mA�oA��A�ȴA�l�A�t�A���A���A�A��A�\)A���A���A�{A�K�A�r�A��-A�z�A��A��PA���A�33A���A� �A�1A��A��#A���A��A��DA���A�(�A���A��A��A33A~v�A|1'Ay��Aw�wAu��ArQ�Ah�AgVAd�A`�AZZAX�yAXbNAX-AX�AW�AV�+AU��AT~�AR�AOp�AKx�AH�HAH5?AF��ACA?|�A<��A9��A8A4��A1�A0ĜA.�yA,�`A+�-A+�A)/A(~�A(v�A(��A(�DA( �A'�-A&9XA%\)A%;dA%�A$��A$��A$ �A"r�A!�A ��A�mA7LA"�AA��A�AA�A��A�A��Av�AQ�A5?A1'A(�A��AM�A�yA�A�A��A��AĜA~�AJA��AffAQ�A�#A��A�A��A�
AXA�An�A�AS�A�HA�A
��A	�TA	�^A	XA	XA	�A�A5?A�A~�A�;AC�AZA`BA^5A;dAAC�A ��A VA   @��R@�hs@�t�@�J@��;@��@���@�J@���@�1'@�J@��@��`@�r�@��y@���@��@�Z@띲@�@���@�J@��T@噚@�7L@䛦@�r�@�I�@��;@�33@�$�@�7@�V@�o@�-@��`@��@ݡ�@�O�@��@�ȴ@�E�@�{@�@��@؃@�&�@ڏ\@�V@�J@��T@�@�X@�j@�I�@�(�@��m@�K�@��@�~�@���@�`B@��@���@��@Լj@� �@ӕ�@���@җ�@�=q@�hs@�Z@�ƨ@�;d@���@ΰ!@�=q@�hs@��@ʸR@�?}@���@ȓu@�1'@ǶF@�\)@��@���@Ƨ�@�V@�=q@�{@��#@š�@�`B@�V@Ĭ@��@�t�@��@�~�@�=q@��@��^@�X@�%@�b@�|�@�v�@���@�?}@���@�1'@�ƨ@���@��;@��@�1'@�(�@�  @��
@�ƨ@���@�33@���@���@�V@���@�O�@�?}@�?}@�?}@�&�@��@�ƨ@��F@��@���@���@�t�@�dZ@�\)@�S�@�C�@�C�@��@��+@�x�@��u@��@��w@�33@��H@���@�n�@�J@���@���@�x�@�`B@�%@��u@��m@��@���@��D@��@�;d@�V@��@�@��@�X@��@��j@�I�@���@�o@��@���@�^5@�-@�{@�$�@�5?@��@��@��@��@�9X@�1'@� �@�b@��m@��P@�t�@�l�@�dZ@�;d@�+@�ȴ@���@��+@�E�@�@�x�@�&�@��`@��9@�z�@�b@���@�C�@�
=@��R@��+@�5?@���@�O�@���@��@�b@��;@��@��@�v�@�V@�{@���@��7@�G�@�/@��@��`@���@�r�@�A�@�b@��;@��P@�dZ@�+@��!@�V@�5?@��@��T@��^@��h@�7L@��@�z�@�9X@���@��;@��@�|�@�\)@�S�@�;d@��@���@�=q@���@���@���@���@��h@��7@�G�@��@���@���@�Q�@� �@��P@�dZ@�S�@�C�@�+@�o@��@��R@�M�@�J@�@���@�p�@�&�@��/@��j@��9@�z�@�bN@��P@�;d@�+@�@���@��@��y@���@�E�@���@���@��@��@��@��@��@�x�@�V@���@�9X@���@���@��w@���@�\)@�@�M�@��#@��@���@�r�@�1'@�(�@���@��FG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�1'A�  AӃA�+A��TAғuAя\A�  AЧ�AЏ\AЅA�x�A�M�A�-A��A�VA���A���A���A���A��yA��mA��A��yA��yA��A��mA��yA��A��mA��yA��yA��HA��TA��`A��TA��A��
A���A���A���A���A���AϸRAϴ9AϮAϬAϲ-AϮAϮAϮAϩ�AϮAϲ-AϬAϮAϲ-AϮAϩ�AϮAϬAϥ�Aϩ�Aϣ�Aϛ�Aϛ�Aϟ�Aϙ�Aϕ�Aϗ�Aϙ�AϓuAϓuAϕ�AϏ\AϏ\AϑhAϋDAω7AϏ\AύPAϋDAύPAϏ\Aω7Aω7AϋDAσAυAυA�|�A�|�AρA�|�A�x�A�|�A�z�A�v�A�|�A�x�A�v�A�|�A�x�A�v�A�z�A�z�A�v�A�z�A�x�A�v�A�z�A�v�A�p�A�n�A�r�A�hsA�bNA�ffA�ffA�bNA�ffA�ffA�bNA�ffA�hsA�bNA�ffA�hsA�dZA�dZA�hsA�hsA�ffA�hsA�l�A�hsA�dZA�jA�jA�ffA�hsA�jA�ffA�hsA�jA�dZA�hsA�jA�jA�dZA�bNA�dZA�ZA�ZA�^5A�VA�XA�\)A�XA�Q�A�M�A�G�A�C�A�?}A�?}A�C�A�C�A�?}A�C�A�G�A�A�A�C�A�I�A�G�A�G�A�K�A�M�A�M�A�M�A�VA�VA�S�A�ZA�\)A�XA�`BA�`BA�dZA�^5A�^5A�jA�jA�hsA�r�A�t�A�r�A�v�A�z�A�t�A�x�A�z�A�x�A�t�A�z�A�z�A�v�A�x�A�|�A�z�A�z�A�~�A�~�A�z�A�|�AρA�z�A�z�A�~�A�x�A�z�A�~�A�z�AρAρA�z�AρA�|�AρAσA�~�AσAρAρAχ+AυAρAσAυAχ+AυAυAω7Aω7Aχ+AύPAϏ\AύPAύPAϙ�Aϗ�Aϗ�Aϛ�Aϟ�Aϝ�Aϛ�Aϡ�Aϣ�Aϣ�Aϥ�AϬAϬAϬAϲ-Aϰ!AϮA϶FAϴ9Aϰ!Aϴ9Aϲ-Aϰ!A϶FAϴ9Aϲ-AϾwAϺ^AϸRA�A���A�A�ƨA���A�A�ƨA�ĜA�A�ȴA�ȴA�ƨA���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���A�ƨA�A�ĜA�AϾwA���A�AϼjAϾwA�A���AϼjAϾwA���A���AϾwAϺ^AϾwAϾwAϼjAϸRAϼjAϼjAϺ^AϺ^AϾwAϺ^AϸRAϸRAϼjAϼjAϸRAϼjAϾwAϸRAϸRAϼjAϾwAϸRAϸRAϺ^AϺ^A϶FAϲ-A϶FA϶FAϲ-AϬAϮAϩ�Aϥ�Aϥ�Aϧ�Aϝ�AϋDA�v�A�l�A�bNA�XA�XA�VA�K�A�?}A�7LA�5?A�/A��A�%A���A���AζFA�r�A�M�A�5?A��A��A��
A���AͮA͡�A͙�A͕�A͋DA�~�A�v�A�r�A�jA�^5A�ZA�XA�O�A�I�A�G�A�G�A�C�A�A�A�C�A�A�A�;dA�7LA��A�A�ȴẠ�Ả7A�~�A�~�A�dZA�ĜA�bNA���Aʝ�A�"�Aɏ\A�1'Aȴ9Aș�Aȉ7A�O�A���AǶFAǑhA�l�A�K�A�33A�
=A���A��A��A���A���AƲ-AƟ�AƉ7A�r�A�hsA�^5A�9XA�
=A��AŮAŅA�`BA�7LA���Aħ�A�n�A��AÙ�A�VA�JA�ĜA\A�\)A�5?A�oA��A�ȴA���A��\A�jA��A��9A�jA�A�A�(�A��A�
=A�%A�A���A��A��yA��HA��/A���A���A��^A��A���A�p�A�A�A�/A��A�oA�A��A��`A���A�ZA��mA���A�A�A��`A��FA���A�l�A�;dA�&�A�
=A��`A�A���A�p�A�ffA�S�A�7LA�(�A� �A��A�1A�  A�  A��A��yA��#A���A��wA��A���A���A��+A�n�A�Q�A�33A��A��A���A��PA��A�~�A�S�A�/A�"�A� �A��A�oA�VA�VA�%A�A��A�ĜA�bNA��A���A���A�l�A�=qA�"�A�  A��;A�ƨA���A�|�A�G�A���A���A��9A���A��\A�~�A�r�A�VA�I�A�/A�(�A��A�VA�
=A�
=A���A���A��TA��-A���A��7A�M�A�I�A�9XA�33A��A�ƨA��\A�r�A�Q�A���A�|�A�l�A���A���A�n�A�XA�$�A���A���A��uA�n�A�`BA�?}A�(�A��A�%A��A��`A��#A���A��9A���A���A��PA�n�A�ZA�O�A�?}A�+A�{A���A��`A���A��!A��PA�E�A���A���A���A�K�A�1A���A��uA�1'A���A�O�A�{A��A���A��RA��-A��A���A��DA�v�A�bNA�A�A���A���A���A�n�A�33A��A���A��jA��9A���A���A��A�&�A�oA��/A���A��wA��!A���A��7A�n�A�ZA�5?A���A��/A���A���A�ƨA���A���A��uA��A�dZA�C�A�$�A���A�ƨA���A�z�A�ffA�5?A�bA��A��A���A��^A��^A��-A��!A��A��A��!A��A��A��A��!A��A���A���A���A���A��7A�v�A�bNA�=qA�&�A�VA�  A���A��A��;A�ĜA���A��A�ffA�A�A�(�A���A��/A��wA���A���A�r�A�M�A�$�A���A��#A��A��\A�p�A�1'A��DA��
A�~�A�(�A��HA��A���A�t�A���A�z�A�(�A�A���A��
A��FA��DA�XA��A���A���A�`BA�JA���A���A��A�t�A�`BA�A�A��A���A��wA�~�A�bNA�G�A�/A�%A�ĜA���A�VA�(�A��A���A�bNA�/A��A�  A��HA�A���A��A�I�A�A��9A��#A��A�{A�ĜA�x�A�C�A���A��`A���A��PA��A�p�A�33A���A���A�bNA�I�A�7LA�(�A�&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                     111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aө�A�~�A���A�z�A�-A���A��A��yA��yA��yA��mA��TA���Aϴ9AϮAϮAϥ�Aϙ�AϑhAύPAω7AρA�x�A�x�A�x�A�l�A�dZA�ffA�hsA�hsA�hsA�ffA�ZA�K�A�A�A�G�A�O�A�ZA�ffA�v�A�z�A�z�A�~�AυAϏ\Aϟ�AϮA϶FA�A�ȴA���A���A���A���AϼjAϺ^AϺ^Aϴ9AϑhA�;dA�bNA͉7A�G�A̼jA��/A�ZA�hsA�x�A��mA�"�A���A�n�A��A�JA�ZA�7LA�{A�5?A�+A�O�A�33A�VA���A��mA�oA��A�ȴA�l�A�t�A���A���A�A��A�\)A���A���A�{A�K�A�r�A��-A�z�A��A��PA���A�33A���A� �A�1A��A��#A���A��A��DA���A�(�A���A��A��A33A~v�A|1'Ay��Aw�wAu��ArQ�Ah�AgVAd�A`�AZZAX�yAXbNAX-AX�AW�AV�+AU��AT~�AR�AOp�AKx�AH�HAH5?AF��ACA?|�A<��A9��A8A4��A1�A0ĜA.�yA,�`A+�-A+�A)/A(~�A(v�A(��A(�DA( �A'�-A&9XA%\)A%;dA%�A$��A$��A$ �A"r�A!�A ��A�mA7LA"�AA��A�AA�A��A�A��Av�AQ�A5?A1'A(�A��AM�A�yA�A�A��A��AĜA~�AJA��AffAQ�A�#A��A�A��A�
AXA�An�A�AS�A�HA�A
��A	�TA	�^A	XA	XA	�A�A5?A�A~�A�;AC�AZA`BA^5A;dAAC�A ��A VA   @��R@�hs@�t�@�J@��;@��@���@�J@���@�1'@�J@��@��`@�r�@��y@���@��@�Z@띲@�@���@�J@��T@噚@�7L@䛦@�r�@�I�@��;@�33@�$�@�7@�V@�o@�-@��`@��@ݡ�@�O�@��@�ȴ@�E�@�{@�@��@؃@�&�@ڏ\@�V@�J@��T@�@�X@�j@�I�@�(�@��m@�K�@��@�~�@���@�`B@��@���@��@Լj@� �@ӕ�@���@җ�@�=q@�hs@�Z@�ƨ@�;d@���@ΰ!@�=q@�hs@��@ʸR@�?}@���@ȓu@�1'@ǶF@�\)@��@���@Ƨ�@�V@�=q@�{@��#@š�@�`B@�V@Ĭ@��@�t�@��@�~�@�=q@��@��^@�X@�%@�b@�|�@�v�@���@�?}@���@�1'@�ƨ@���@��;@��@�1'@�(�@�  @��
@�ƨ@���@�33@���@���@�V@���@�O�@�?}@�?}@�?}@�&�@��@�ƨ@��F@��@���@���@�t�@�dZ@�\)@�S�@�C�@�C�@��@��+@�x�@��u@��@��w@�33@��H@���@�n�@�J@���@���@�x�@�`B@�%@��u@��m@��@���@��D@��@�;d@�V@��@�@��@�X@��@��j@�I�@���@�o@��@���@�^5@�-@�{@�$�@�5?@��@��@��@��@�9X@�1'@� �@�b@��m@��P@�t�@�l�@�dZ@�;d@�+@�ȴ@���@��+@�E�@�@�x�@�&�@��`@��9@�z�@�b@���@�C�@�
=@��R@��+@�5?@���@�O�@���@��@�b@��;@��@��@�v�@�V@�{@���@��7@�G�@�/@��@��`@���@�r�@�A�@�b@��;@��P@�dZ@�+@��!@�V@�5?@��@��T@��^@��h@�7L@��@�z�@�9X@���@��;@��@�|�@�\)@�S�@�;d@��@���@�=q@���@���@���@���@��h@��7@�G�@��@���@���@�Q�@� �@��P@�dZ@�S�@�C�@�+@�o@��@��R@�M�@�J@�@���@�p�@�&�@��/@��j@��9@�z�@�bN@��P@�;d@�+@�@���@��@��y@���@�E�@���@���@��@��@��@��@��@�x�@�V@���@�9X@���@���@��w@���@�\)@�@�M�@��#@��@���@�r�@�1'@�(�@���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1'A�1'A�  AӃA�+A��TAғuAя\A�  AЧ�AЏ\AЅA�x�A�M�A�-A��A�VA���A���A���A���A��yA��mA��A��yA��yA��A��mA��yA��A��mA��yA��yA��HA��TA��`A��TA��A��
A���A���A���A���A���AϸRAϴ9AϮAϬAϲ-AϮAϮAϮAϩ�AϮAϲ-AϬAϮAϲ-AϮAϩ�AϮAϬAϥ�Aϩ�Aϣ�Aϛ�Aϛ�Aϟ�Aϙ�Aϕ�Aϗ�Aϙ�AϓuAϓuAϕ�AϏ\AϏ\AϑhAϋDAω7AϏ\AύPAϋDAύPAϏ\Aω7Aω7AϋDAσAυAυA�|�A�|�AρA�|�A�x�A�|�A�z�A�v�A�|�A�x�A�v�A�|�A�x�A�v�A�z�A�z�A�v�A�z�A�x�A�v�A�z�A�v�A�p�A�n�A�r�A�hsA�bNA�ffA�ffA�bNA�ffA�ffA�bNA�ffA�hsA�bNA�ffA�hsA�dZA�dZA�hsA�hsA�ffA�hsA�l�A�hsA�dZA�jA�jA�ffA�hsA�jA�ffA�hsA�jA�dZA�hsA�jA�jA�dZA�bNA�dZA�ZA�ZA�^5A�VA�XA�\)A�XA�Q�A�M�A�G�A�C�A�?}A�?}A�C�A�C�A�?}A�C�A�G�A�A�A�C�A�I�A�G�A�G�A�K�A�M�A�M�A�M�A�VA�VA�S�A�ZA�\)A�XA�`BA�`BA�dZA�^5A�^5A�jA�jA�hsA�r�A�t�A�r�A�v�A�z�A�t�A�x�A�z�A�x�A�t�A�z�A�z�A�v�A�x�A�|�A�z�A�z�A�~�A�~�A�z�A�|�AρA�z�A�z�A�~�A�x�A�z�A�~�A�z�AρAρA�z�AρA�|�AρAσA�~�AσAρAρAχ+AυAρAσAυAχ+AυAυAω7Aω7Aχ+AύPAϏ\AύPAύPAϙ�Aϗ�Aϗ�Aϛ�Aϟ�Aϝ�Aϛ�Aϡ�Aϣ�Aϣ�Aϥ�AϬAϬAϬAϲ-Aϰ!AϮA϶FAϴ9Aϰ!Aϴ9Aϲ-Aϰ!A϶FAϴ9Aϲ-AϾwAϺ^AϸRA�A���A�A�ƨA���A�A�ƨA�ĜA�A�ȴA�ȴA�ƨA���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���A�ƨA�A�ĜA�AϾwA���A�AϼjAϾwA�A���AϼjAϾwA���A���AϾwAϺ^AϾwAϾwAϼjAϸRAϼjAϼjAϺ^AϺ^AϾwAϺ^AϸRAϸRAϼjAϼjAϸRAϼjAϾwAϸRAϸRAϼjAϾwAϸRAϸRAϺ^AϺ^A϶FAϲ-A϶FA϶FAϲ-AϬAϮAϩ�Aϥ�Aϥ�Aϧ�Aϝ�AϋDA�v�A�l�A�bNA�XA�XA�VA�K�A�?}A�7LA�5?A�/A��A�%A���A���AζFA�r�A�M�A�5?A��A��A��
A���AͮA͡�A͙�A͕�A͋DA�~�A�v�A�r�A�jA�^5A�ZA�XA�O�A�I�A�G�A�G�A�C�A�A�A�C�A�A�A�;dA�7LA��A�A�ȴẠ�Ả7A�~�A�~�A�dZA�ĜA�bNA���Aʝ�A�"�Aɏ\A�1'Aȴ9Aș�Aȉ7A�O�A���AǶFAǑhA�l�A�K�A�33A�
=A���A��A��A���A���AƲ-AƟ�AƉ7A�r�A�hsA�^5A�9XA�
=A��AŮAŅA�`BA�7LA���Aħ�A�n�A��AÙ�A�VA�JA�ĜA\A�\)A�5?A�oA��A�ȴA���A��\A�jA��A��9A�jA�A�A�(�A��A�
=A�%A�A���A��A��yA��HA��/A���A���A��^A��A���A�p�A�A�A�/A��A�oA�A��A��`A���A�ZA��mA���A�A�A��`A��FA���A�l�A�;dA�&�A�
=A��`A�A���A�p�A�ffA�S�A�7LA�(�A� �A��A�1A�  A�  A��A��yA��#A���A��wA��A���A���A��+A�n�A�Q�A�33A��A��A���A��PA��A�~�A�S�A�/A�"�A� �A��A�oA�VA�VA�%A�A��A�ĜA�bNA��A���A���A�l�A�=qA�"�A�  A��;A�ƨA���A�|�A�G�A���A���A��9A���A��\A�~�A�r�A�VA�I�A�/A�(�A��A�VA�
=A�
=A���A���A��TA��-A���A��7A�M�A�I�A�9XA�33A��A�ƨA��\A�r�A�Q�A���A�|�A�l�A���A���A�n�A�XA�$�A���A���A��uA�n�A�`BA�?}A�(�A��A�%A��A��`A��#A���A��9A���A���A��PA�n�A�ZA�O�A�?}A�+A�{A���A��`A���A��!A��PA�E�A���A���A���A�K�A�1A���A��uA�1'A���A�O�A�{A��A���A��RA��-A��A���A��DA�v�A�bNA�A�A���A���A���A�n�A�33A��A���A��jA��9A���A���A��A�&�A�oA��/A���A��wA��!A���A��7A�n�A�ZA�5?A���A��/A���A���A�ƨA���A���A��uA��A�dZA�C�A�$�A���A�ƨA���A�z�A�ffA�5?A�bA��A��A���A��^A��^A��-A��!A��A��A��!A��A��A��A��!A��A���A���A���A���A��7A�v�A�bNA�=qA�&�A�VA�  A���A��A��;A�ĜA���A��A�ffA�A�A�(�A���A��/A��wA���A���A�r�A�M�A�$�A���A��#A��A��\A�p�A�1'A��DA��
A�~�A�(�A��HA��A���A�t�A���A�z�A�(�A�A���A��
A��FA��DA�XA��A���A���A�`BA�JA���A���A��A�t�A�`BA�A�A��A���A��wA�~�A�bNA�G�A�/A�%A�ĜA���A�VA�(�A��A���A�bNA�/A��A�  A��HA�A���A��A�I�A�A��9A��#A��A�{A�ĜA�x�A�C�A���A��`A���A��PA��A�p�A�33A���A���A�bNA�I�A�7LA�(�A�&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                     111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��B(�B3hB1�B(�B'�B#:B!�B 'B \B �B!bB �BVB�B�B~BBqB	BkB	B�B1B1B�B�BBBBBBkB�BkB�BIB�B"�B%zB,B/�B2�B5�B8RB:�B?BB�BEBH�BK�BO�BS[BT�BT�BVmBW�BW�BW�BX�Ba�BzDB��B��B��B�!B�kB�eB�B��B�B��BʌB��B�B!-B,=B4�B@OBA BD3BOBP�BOBP}B^BVmBOBBH�BI�BFBD3B;�B9$B9�B4nB,=B"hB�B�;B�/B�^B��B��B��Bp�B]/BM�B>wB%�B
�B
�B
бB
��B
��B
��B
��B
|�B
s�B
iyB
b�B
[WB
C�B
4�B
"hB
�B	�B	ɆB	�}B	�B	��B	�SB	��B	�B	~�B	|�B	yrB	t�B	n�B	gmB	^jB	OvB	?�B	:�B	4nB	-�B	 �B	FB	
�B	�B	�B��B�B�B�8B�fB	�B	~B	�B	�B	�B	�B	&LB	+�B	@�B	GB	GB	G�B	HKB	GzB	PHB	^�B	d&B	l"B	p�B	u�B	v`B	xlB	}VB	}VB	�B	��B	�B	��B	��B	��B	�"B	��B	�~B	��B	�	B	��B	��B	�XB	��B	�eB	�UB	��B	�9B	�0B	��B	�$B	��B	��B	ȀB	��B	�HB	��B	ϫB	�B	�B	�HB	ΥB	�<B	՛B	�}B	�B	�B	�QB	�B	ںB	خB	��B	��B	�2B	уB	˒B	�RB	B	�qB	ɺB	ɆB	B	��B	��B	�B	�XB	�3B	�B	��B	�OB	��B	�B	�qB	��B	��B	�B	�6B	�B	�HB	�HB	��B	�B	��B	ƨB	�)B	ȴB	ȀB	�EB	��B	ŢB	�B	�9B	ŢB	��B	ŢB	ŢB	ǮB	�9B	�3B	��B	�3B	�B	�<B	ϫB	�vB	�BB	�B	�NB	ԕB	�B	��B	�WB	�iB	�B	�5B	�5B	��B	�B	�iB	��B	�B	�ZB	��B	��B	��B	��B	�B	��B	��B	�B	��B	�.B	��B	��B	��B	��B	��B	�B	��B	�B	�>B	��B	�8B	�lB	�DB	�B	�xB	�B	�JB	�B	��B	�PB	�B	��B	��B	�PB	�PB	��B	��B	�PB	�B	�B	�B	�B	�PB	��B	��B	�"B	��B	�VB	��B
�B
�B
�B
GB
{B
�B
1B
B
�B
"B
�B
B
B
�B
_B
+B
_B
1B
B
1B
�B
�B
�B
�B
�B
YB
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
MB
MB
SB
MB
SB
uB
B
@B
�B
�B
oB
oB
oB
:B
oB
�B
oB
B
B
�B
B
�B
�B
�B
 B
:B
�B
hB
�B
hB
hB
�B
B
B
@B
uB
uB
B
FB
FB
�B
�B
_B
kB
�B
	B
�B
�B
=B
xB
�B
 �B
#:B
$�B
&�B
'�B
($B
*�B
*�B
+kB
+�B
-B
-�B
/OB
/�B
0�B
0�B
2-B
33B
33B
33B
3�B
3�B
49B
4nB
5B
5�B
6FB
7B
6�B
7�B
8�B
9$B
9�B
9�B
:*B
:�B
:�B
:�B
;0B
;�B
<B
<B
<6B
<�B
<�B
=<B
=<B
=qB
>�B
?B
?B
?HB
?�B
@OB
@�B
A�B
A�B
A�B
B[B
B�B
B�B
C-B
C�B
D3B
DgB
D3B
EmB
EmB
FtB
F?B
F�B
F�B
F�B
FtB
FtB
GEB
GEB
GEB
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K)B
K)B
K^B
K�B
L0B
L0B
K�B
L0B
L0B
NpB
N<B
N<B
N�B
N<B
NB
NB
NB
OvB
OBB
OvB
OBB
OB
OB
OB
N�B
O�B
PB
P�B
QNB
Q�B
Q�B
Q�B
Q�B
R B
RTB
S�B
TaB
U�B
UgB
VmB
VmB
VmB
W
B
W?G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BVB�B)�B/�B#B-�B5tBCaB33B1�B'�B(�B'B-CB'B&�B$@B"�B#:B!�B�B"4B!-B�B �B \B�B!�B �B 'B!bB �B 'B"hB!-B�B �B"�B \B�B�B�B�B�BVB~BBCB7BBBB�B�BCB�BIBCB�B�BxBIB�B�B�BB�B	B�BCB=BBxBqB�B	B=B1B�B�B�BkB�B	B7BBCBB�B�BB=B7B_B�B7B_B_B7B�B1BeB�B�B7B�B�B�B�BeBkB�B�BqB	B+B7BCB1B�B�B�BeB	B�BeB	BB1B�B�B�B�B�BB�B�BqB1B1B=BB1B�B7B�B=B7B�B�B=BBkB�BBB=B7B�BBB7B1B�B�B�BkBqB�B�BqB�BBCB�B!B�BB�B�B�B�B!bB!�B!bB#�B#:B"�B"4B$tB#�B#�B&�B(�B(�B(XB+6B,B+�B-�B,�B,=B,�B.B,qB-CB/B/�B.B0!B3hB1�B2aB3�B2�B1�B49B2�B1[B3hB2�B1�B33B33B3hB5�B33B6B6�B6FB8�B6�B8B8B6zB8RB9�B8�B8B7�B8�B:*B8�B8�B;0B9�B9XB:�B<jB:�B=<B>�B>BB<�B>B?�B>�B>wBA BB'B@BA�BB�BA�BC-BD3BB�BDgBE�BC�BD�BEmBC�BD�BE�BF?BGEBG�BEBH�BI�BHKBI�BI�BH�BI�BJ�BI�BJ�BL�BJ�BK�BNBM6BLdBOvBPBOBBPHBQ�BOvBP�BS[BR�BR BR BT,BT�BS[BR�BS�BU�BTaBS�BU2BUgBS�BT�BVBS�BTaBVBT�BS�BVBU�BT�BT�BVmBVmBU2BT�BV9BW?BU�BU�BV�BXyBW
BV�BXEBW�BV9BW�BX�BX�BW
BWsBYBWsBV�BU2BXyBW
BV�BXyBX�BV�BW
BXyBX�BW
BV�BW�BYKBW�BWsBX�BX�BW
BXEBZ�BY�BWsBYKB\)B[�B[�B_BcTBd�Bc�BcTBh>BjBj�BjKBzDBuZBzBy�B.B��B��B�JB��B��B��B��B�.B��B��B�4B�oB�B�@B�oB�uB�B�:B��B�B�B�4B�hB��B��B��B��B��B�B��B��B��B��B��B��B�-B��B�!B��B��B�RB�B��B�_B�CB�}B��B�=B�6B��B��B��B�_B�eB��B�RB�$B�_B�0B��B��B��B�B��B��B��B��B��B�9B�9B��B��B��B��B��B��B�*B�B�dB��B��B��B�XB�tB�B�B��B��B�XB�B��B�3B��B�aB��B�[B�[B��B�hB�aB�-B�-B�?B��B�LB��B�^B�dB�B�B��B�gB��B�zBуB�HB��B�B�BB��B�B��B��B�%B�BB�B�B�B:BoB�B�B�BSB_BYB�B�B�B=BqB�BIBkB�B�B!B!bB#�B$�B*�B+�B.IB)*B)_B1�B/�B-�B+6B-B-CB+�B*eB+kB*�B+kB2aB>�B5?B1�B6FBE9B6�B7�B;dB;dB=B>BB>BE9BF�BA�BC-B?�BA�B>�B>�BE�BA BGB>wB@B@�B?}B=�B?}B?HBC�BF�B?HBC-BLdB>�BAUB@�BM�BD�BJ�BB�BHKBZBI�BHBXBU2BM6BNpBOBBQ�BS&BQ�BP�BP}BQ�BO�BN�BOvBO�BOBBMjBM�BPHBN�BM�BOBBQ�BO�BM�BNBOvBP�BOBBO�BP�BQ�BR BVBY�BU�BY�B]/B\�B[�Ba�Be�Bm�B^�B\�BXyBT�BT�BR BQ�BQBPHBP�BOBBR�BS�BN<BMjBK�BL0BQ�BK^BGBEmBFBF?BH�BQNBD�BPBI�BHBG�BG�BK)BJ#BF�BL0BJ�BI�BE�BB�BHBJ�BE9BC�BD�BIBIRBB�BH�BF�BHB?}B@OBGB@OB?B9XB<jB;0B9�B8RB;0B:�B9�B8RB8�B9�B9XB8B8�B8�B;�B9�B9�B9�B<�B;�B;dB8�B7LB7�B5tB4nB5?B4�B6�B4B33B2-B0�B/B0!B+�B+6B'RB+�B(�B)�B$�B$tB#nBIB!B!B5BqB�B�B�B�B �B;B@BB  B�|B�B�B�oB��B�/B�B�]B��B�KB�8BߤBޞBخB�B�,B�BҽB��B҉B��BȀB�B�3BƨB�B�jB�B�zB��B�RB�B��B�_B�B��B�zB�B��B�B��B�VB��B��B�LB��B|�B}�B}"BtBt�Bp�Bn�Bl�Bu%Bt�BffBm�Bd&B]�B^BZ�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                     444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                     444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2022010506463120220105064631IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022011423330620220114233306QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022011423330620220114233306QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365720220126093657IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295920220204232959IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295920220204232959IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295920220204232959IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                